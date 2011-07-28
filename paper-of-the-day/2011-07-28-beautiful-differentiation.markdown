I'll need a lot longer than 30 minutes to fully digest this:

A quick summary goes like this. But bundling up a function and its derivative together one can perform automatic differentiation when one combines two functions together. First order differentiation over scalar spaces is easy.

Conal extends this work so that

- it's higher order (you can differentiate as many times as you want)

- it's over more complex domains, in this case arbitrary vector spaces. In private discussions Conal has mentioned to me that he learned about a beautiful formulation of differentiation while he was still at university -- the Calculus of Manifolds. Spivek is referenced in the paper.

The beautiful thing about this paper is that it is all derived. He makes use of his type class morphisms idea to aid in derivation.

There is also some fascinating stuff on memo tries and how that can be used to make the implementation more efficient. I'm going to have to have to read his blog posts on that some time.

He is extending the work of Jerzy Karczmarczuk who I remember André Pang describing as a "bit crazy" when we went to ICFP in 2003.
