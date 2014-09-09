
~~~{.haskell}
⋀s s'. 
⟦cpspace_relation (ksPSpace s) (underlying_memory (ksMachineState s)) (t_hrs_' (globals s'));
 cready_queues_relation (cslift s') (ksReadyQueues_' (globals s')) (ksReadyQueues s);
 ksCurThread_' (globals s') = tcb_ptr_to_ctcb_ptr (ksCurThread s);
 ksIdleThread_' (globals s') = tcb_ptr_to_ctcb_ptr (ksIdleThread s);
 cinterrupt_relation (ksInterruptState s) (intStateIRQNode_' (globals s'))
   (intStateIRQTable_' (globals s'));
 case ksSchedulerAction s of 
     ResumeCurrentThread ⇒ ksSchedulerAction_' (globals s') = tcb_Ptr 0
   | ChooseNewThread ⇒ ksSchedulerAction_' (globals s') = tcb_Ptr (~~ 0)
   | SwitchToThread p' ⇒ ksSchedulerAction_' (globals s') = tcb_ptr_to_ctcb_ptr p';
 carch_state_relation (ksArchState s) (globals s');
 cmachine_state_relation (ksMachineState s) (globals s');
 ghost'state_' (globals s') = (gsUserPages s, gsCNodes s);
 ksWorkUnitsCompleted_' (globals s') = ksWorkUnitsCompleted s;
 s' ⊨\<^sub>c adglobs_addr;
 globals_list_id_fudge globals_list_h_t_valid (hrs_htd (t_hrs_' (globals s'))) local.globals_list;
 ptr_span adglobs_addr ⊆ kernel_data_refs;
 globals_list_footprint local.globals_list ⊆ kernel_data_refs;
 weak_sch_act_wf (ksSchedulerAction s) s
⟧

⟹ 

(∃t. ksSchedulerAction s = SwitchToThread t) =
  (ksSchedulerAction_' (globals s') ≠ tcb_Ptr 0 ∧
   ksSchedulerAction_' (globals s') ≠ tcb_Ptr max_word)

⋀s s'.
⟦cpspace_relation (ksPSpace s) (underlying_memory (ksMachineState s)) (t_hrs_' (globals s'));
⟦cpspace_relation (ksPSpace s) (underlying_memory (ksMachineState s)) (t_hrs_' (globals s'));
 cready_queues_relation (cslift s') (ksReadyQueues_' (globals s')) (ksReadyQueues s);
 cready_queues_relation (cslift s') (ksReadyQueues_' (globals s')) (ksReadyQueues s);
 ksCurThread_' (globals s') = tcb_ptr_to_ctcb_ptr (ksCurThread s);
 ksCurThread_' (globals s') = tcb_ptr_to_ctcb_ptr (ksCurThread s);
 ksIdleThread_' (globals s') = tcb_ptr_to_ctcb_ptr (ksIdleThread s);
 ksIdleThread_' (globals s') = tcb_ptr_to_ctcb_ptr (ksIdleThread s);
 cinterrupt_relation (ksInterruptState s) (intStateIRQNode_' (globals s'))
   (intStateIRQTable_' (globals s'));
 cinterrupt_relation (ksInterruptState s) (intStateIRQNode_' (globals s'))
   (intStateIRQTable_' (globals s'));
 case ksSchedulerAction s of
     ResumeCurrentThread ⇒ ksSchedulerAction_' (globals s') = tcb_Ptr 0
   | ChooseNewThread ⇒ ksSchedulerAction_' (globals s') = tcb_Ptr (~~ 0)
   | SwitchToThread p' ⇒ ksSchedulerAction_' (globals s') = tcb_ptr_to_ctcb_ptr p';

 case ksSchedulerAction s of 
     ResumeCurrentThread ⇒ ksSchedulerAction_' (globals s') = tcb_Ptr 0
   | ChooseNewThread ⇒ ksSchedulerAction_' (globals s') = tcb_Ptr (~~ 0)
   | SwitchToThread p' ⇒ ksSchedulerAction_' (globals s') = tcb_ptr_to_ctcb_ptr p';


 carch_state_relation (ksArchState s) (globals s');
 cmachine_state_relation (ksMachineState s) (globals s');
 ghost'state_' (globals s') = (gsUserPages s, gsCNodes s);
 ksWorkUnitsCompleted_' (globals s') = ksWorkUnitsCompleted s;
 s' ⊨\<^sub>c adglobs_addr;
 globals_list_id_fudge globals_list_h_t_valid (hrs_htd (t_hrs_' (globals s'))) local.globals_list;
 ptr_span adglobs_addr ⊆ kernel_data_refs;
 globals_list_footprint local.globals_list ⊆ kernel_data_refs;
 weak_sch_act_wf (ksSchedulerAction s) s

⋀s s'. 





 carch_state_relation (ksArchState s) (globals s');
 cmachine_state_relation (ksMachineState s) (globals s');
 ghost'state_' (globals s') = (gsUserPages s, gsCNodes s);
 ksWorkUnitsCompleted_' (globals s') = ksWorkUnitsCompleted s;
 s' ⊨\<^sub>c adglobs_addr;
 globals_list_id_fudge globals_list_h_t_valid (hrs_htd (t_hrs_' (globals s'))) local.globals_list;
 ptr_span adglobs_addr ⊆ kernel_data_refs;
 globals_list_footprint local.globals_list ⊆ kernel_data_refs;
 weak_sch_act_wf (ksSchedulerAction s) s
⟧

⟹ 

(∃t. ksSchedulerAction s = SwitchToThread t) =
  (ksSchedulerAction_' (globals s') ≠ tcb_Ptr 0 ∧
   ksSchedulerAction_' (globals s') ≠ tcb_Ptr max_word)

⋀s s'.
⟦cpspace_relation (ksPSpace s) (underlying_memory (ksMachineState s)) (t_hrs_' (globals s'));
 cready_queues_relation (cslift s') (ksReadyQueues_' (globals s')) (ksReadyQueues s);
 ksCurThread_' (globals s') = tcb_ptr_to_ctcb_ptr (ksCurThread s);
 ksIdleThread_' (globals s') = tcb_ptr_to_ctcb_ptr (ksIdleThread s);
 cinterrupt_relation (ksInterruptState s) (intStateIRQNode_' (globals s'))
   (intStateIRQTable_' (globals s'));
 case ksSchedulerAction s of
     ResumeCurrentThread ⇒ ksSchedulerAction_' (globals s') = tcb_Ptr 0
   | ChooseNewThread ⇒ ksSchedulerAction_' (globals s') = tcb_Ptr (~~ 0)
   | SwitchToThread p' ⇒ ksSchedulerAction_' (globals s') = tcb_ptr_to_ctcb_ptr p';
 carch_state_relation (ksArchState s) (globals s');
 cmachine_state_relation (ksMachineState s) (globals s');
 ghost'state_' (globals s') = (gsUserPages s, gsCNodes s);
 ksWorkUnitsCompleted_' (globals s') = ksWorkUnitsCompleted s;
 s' ⊨\<^sub>c adglobs_addr;
 globals_list_id_fudge globals_list_h_t_valid (hrs_htd (t_hrs_' (globals s'))) local.globals_list;
 ptr_span adglobs_addr ⊆ kernel_data_refs;
 globals_list_footprint local.globals_list ⊆ kernel_data_refs;
 weak_sch_act_wf (ksSchedulerAction s) s
⟧ 

⟹ 

(∃t. ksSchedulerAction s = SwitchToThread t) =
  (ksSchedulerAction_' (globals s') ≠ tcb_Ptr 0 ∧
   ksSchedulerAction_' (globals s') ≠ tcb_Ptr max_word)

~~~