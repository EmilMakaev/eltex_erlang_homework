-record(state, {
    children = [] :: [{atom(), pid()}],
    permanent = [] :: [pid()]
}).
