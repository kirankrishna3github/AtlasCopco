*----------------------------------------------------------------------*
*   INCLUDE SCHEDMAN_EVENTS                                            *
*----------------------------------------------------------------------*
constants: begin of cs_wf_events,
           finished type swo_event value 'FINISHED',
           error    type swo_event value 'REPORTERROR',
           end   of cs_wf_events.
