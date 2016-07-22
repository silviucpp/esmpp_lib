-author("silviu").

%connection state record

-record(conn_state, {
    socket,
    listen_pid,
    processing_pid,
    connection_pid,
    sar,
    seq_n,
    %smpp session info's
    host,
    port,
    password,
    system_id,
    transport,
    interface_version,
    enquire_timeout,
    submit_timeout,
    system_type,
    service_type,
    addr_ton,
    addr_npi,
    source_addr_ton,
    source_addr_npi,
    dest_addr_ton,
    dest_addr_npi,
    mode,
    data_coding,
    handler_pid
}).