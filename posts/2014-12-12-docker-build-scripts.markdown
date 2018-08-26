---
title: File system snapshots make build scripts easy
category: tools
tags: Docker,
date: 2001-01-01
---
## or, how Docker can relieve the pain of developing long running build scripts


I think I've found a pretty compelling use case for Docker.
But before you think that this is yet another blog post parroting the virtues of
Docker I'd like to make clear that this post is really about the virtues of treating your file system as a *persistent data structure*. Thus, the insights of this post are equally applicable to other
[copy-on-write](http://en.wikipedia.org/wiki/Copy-on-write) filesystems such as [btrfs](http://en.wikipedia.org/wiki/Btrfs), and [ZFS](http://en.wikipedia.org/wiki/ZFS).


## The problem

Let's start with the problem I was trying to solve. I was developing a long running build script that consisted of numerous steps.

* It took 1-2 hours to run.
* It downloaded many fairly large files from the Internet. (One exceeded 300M.)
* Later stages depended heavily on libraries built in earlier stages.

But the most salient feature was that it took a long time to run.

## Filesystems are inherently stateful

We typically interact with filesystems in a stateful way. We might add, delete
or move a file. We might change a file's permissions or its access times. In isolation most actions can be undone. e.g. you can move a file back to its original location after having moved it somewhere else.
What we don't typically do is take a snapshot and revert back to that state. This post will suggest
that making more use of this feature can be a great boon to developing long running build scripts.

## Snapshots using union mounts

Docker uses what is called a *union filesystem* called [AUFS](http://en.wikipedia.org/wiki/Aufs).
A union filesystem implements what is known as a
[union mount](http://en.wikipedia.org/wiki/Union_mount). As the name suggests this means
that files and directories of separate file systems are layered on top of each other forming a
single coherent file system. This is done in a hierarchical manner. If a file appears in two
filesystems the one further up the hierarchy will be the one presented. (The version of the file
further down the hierarchy is there, unchanged, but invisible.)

Docker calls each filesystem in the union mount a [layer](https://docs.docker.com/terms/layer).
The upshot of using this technology is that it implements snapshots as a side effect.
Each snapshot is a simply a union mount of all the layers up to a certain point in the hierarchy.

## Snapshots for build scripts

Snapshots make developing a long-running build script a dream. The general idea is to break up the
script up into smaller scripts (which I like to call *scriptlets*) and run each one individually,
snapshotting the filesystem after each one is run. (Docker does this automatically.)
If you find that a scriptlet fails, one simply has to go back to the last snapshot (still in its
pristine state!) and try again. Once you have completed your build script you have a very high
assurance that the script works and can now be distributed to others.

Constrast this with what would happen if you weren't using snapshots. Except for those among us
with monk-like patience, no one is going to going to run their build script from scratch when
it fails an hour and a half into building. Naturally, we'll try our best to put the system
back into the state it was in before we try to build the component that failed last time. e.g. we
might delete a directory or run a <code>make clean</code>.

However, we might not have perfect understanding of the component we're trying to build. It might have a complicated <code>Makefile</code> that puts files in places on the file system which we are unaware of. The only way to be *truly* sure is to revert to a snapshot.

## Using Docker for snapshotted build scripts

In this section I'll cover how I used Docker to implement a build script for a
[GHC](http://haskell.org/ghc) 7.8.3 ARM cross compiler. Docker was pretty good for this task, but
not perfect. I did some things that might look wasteful or inelegant but were necessary in order
to keep the total time developing the script to a minimum. The build script can be found
[here](https://github.com/sseefried/docker-build-ghc-android).

### Building with a <code>Dockerfile</code>

Docker reads from a file called <code>Dockerfile</code> to build images. A <code>Dockerfile</code> contains a small vocabulary of *commands* to specify what actions should be performed.
A complete reference can be found [here](https://docs.docker.com/reference/builder/). The main
ones used in my script are <code>WORKDIR</code>, <code>ADD</code>, and <code>RUN</code>. The
<code>ADD</code> command is particularly useful because it allows you to add files that *external* to
the current Docker image *into* the image's filesystem before running them. You can see the
many scriptlets that make up the build script [here](https://github.com/sseefried/docker-build-ghc-android/tree/master/user-scripts).

### Design

#### 1. <code>ADD</code> scriptlets just before you <code>RUN</code> them.

If you <code>ADD</code> all the scriptlets too early in the <code>Dockerfile</code> you may run into the following
problem: your script fails, you go back to modify the scriptlet and you run <code>docker build .</code>
again. But you find that Docker starts building at the point where the scriptlets were first
added! This wastes a lot of time and defeats the purpose of using snapshots.

The reason this happens is because of how Docker tracks its intermediate images (snapshots).
As Docker steps through the <code>Dockerfile</code> it compares the current command with
an intermediate image to see if there is a match. However, in the case of the <code>ADD</code>
command the contents of the files being put into the image are also examined. This makes sense.
If the files
have changed with respect to an existing intermediate image Docker has no choice but to build a new
image from that point onwards. There's just no way it can know that those changes don't affect
the build. Even if they wouldn't it must be conservative.

Also, beware using <code>RUN</code> commands that would cause different changes to the filesystem
each time they are run. In this case Docker *will* find the intermediate image and use it, but
this will be the wrong thing for it to do. <code>RUN</code> commands must cause the same
change to the filesystem each time they are run. As an example, I ensured that in my scriptlets I
always downloaded a known version of a file with a specific MD5 checksum.

A more detailed explanation of Docker's *build cache* can be found [here](https://docs.docker.com/articles/dockerfile_best-practices/#build-cache).

#### 2.  Don't use the <code>ENV</code> command to set environment variables. Use a scriptlet.

It may seem tempting to use the <code>ENV</code> command to set up all the environment variables
you need for your build script. However, it does not perform variable substitution the way
a shell would. e.g. <code>ENV BASE=\$HOME/base</code> will set <code>BASE</code> to have the
literal value <code>$HOME/base</code> which is probably not what you want.

Instead I used the <code>ADD</code> command to add a file called
[<code>set-env.sh</code>](https://github.com/sseefried/docker-build-ghc-android/blob/master/user-scripts/set-env.sh).
This file is included in each subsequent scriptlet with:


    THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    source $THIS_DIR/set-env-1.sh

What if you don't get <code>set-env.sh</code> right the first time around? Since it's added
so early in the <code>Dockerfile</code> doesn't this mean that modifying it would invalidate
and subsequent snapshots?

Yes, and this leads to some inelegance. While developing the script I discovered that I'd missed
adding a useful environment variable in <code>set-env.sh</code>. The solution was to create a
new file [<code>set-env-1.sh</code>](https://github.com/sseefried/docker-build-ghc-android/blob/master/user-scripts/set-env-1.sh) containing:

    THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    source $THIS_DIR/set-env.sh

    if ! [ -e "$CONFIG_SUB_SRC/config.sub" ] ; then
        CONFIG_SUB_SRC=${CONFIG_SUB_SRC:-$NCURSES_SRC}
    fi

I then included this file in all subsequent scriptlets. Now that I have finished the build script
I could go back and fix this up but, in a sense, it would defeat the purpose. I would have to
run the build script from scratch to see if this change worked.

## Drawbacks

The one major drawback to this approach is that the resulting image is larger than it needs to be.
This is especially true in my case because I remove a large number of files at the end.
However, these files are still present in a lower layer filesystem in the union mount, so the
entire image is larger than it needs to be by at least the size of the removed files.

However, there is a work-around. I did *not* publish this image to the
[Docker Hub Registry](https://registry.hub.docker.com/). Instead, I:

* used <code>docker export</code> to export the contents as a tar archive.
* created a *new* <code>Dockerfile</code> that simply added the contents of this tar archive.

The resulting image was as small as it could be.

## Conclusion

The advantage of this approach is two-fold:

* it keeps development time to a minimum. No longer do you have to sit through builds of
  sub-components that you already know succeed. You can focus on the bits that are still giving you
  grief.

* it is great for *maintaining* a build script. There is a chance that the odd <code>RUN</code>
  command changes its behaviour over time (even though it shoudn't). The build may fail
  but at least you don't have to go back to the beginning once you've fixed the <code>Dockerfile</code>

Also, as I alluded to earlier in the post Docker only makes writing these build scripts *easier*.
With the right tooling the same could be accomplished in any file system that provides snapshots.

Happy building!