-type socket() :: port().
-type server_ref() :: pid() | atom() | {atom(), node()} | {global, term()}.
-type gen_tcpd_socket() :: {gen_tcp | ssl, socket()}.
-type ip_address() :: {byte(), byte(), byte(), byte()} |
	{
		0..65535, 0..65535, 0..65535, 0..65535,
		0..65535, 0..65535, 0..65535, 0..65535
	}.
