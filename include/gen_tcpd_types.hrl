-type server_ref() :: pid() | {atom(), atom()} | {global, term()}.
-type gen_tcpd_socket() :: {atom(), _}.
-type ip_address() :: {byte(), byte(), byte(), byte()} |
	{
		0..65535, 0..65535, 0..65535, 0..65535,
		0..65535, 0..65535, 0..65535, 0..65535
	}.
