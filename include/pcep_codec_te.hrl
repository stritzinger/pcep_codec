
-ifndef(PCEP_CODEC_TE_INCLUDED).
-define(PCEP_CODEC_TE_INCLUDED, true).

%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type ip4_prefix() :: {inet:ip4_address(), 0..32}.
-type ip6_prefix() :: {inet:ip6_address(), 0..128}.

-type te_metric_type() :: igp | te | hop_count | aggregate_bw
                        | most_loaded_link | cumulative_igp | cumulative_te
                        | segment_id_depth.

-type srte_nai_type() :: absent
                       | ipv4_node
                       | ipv6_node
                       | ipv4_adjacency
                       | ipv6_adjacency
                       | unumbered_ipv4_adjacency
                       | linklocal_ipv6_adjacency.

-type te_opstatus() :: down
                     | up
                     | active
                     | going_down
                     | going_up.

-type srte_sid() :: non_neg_integer().


%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(mpls_stack_entry, {
    label = 0 :: 0..1048575
}).

-record(mpls_stack_entry_ext, {
    label = 0 :: 0..1048575,
    tc  = 0 :: 0..7,
    is_last = false :: boolean(),
    ttl = 0 :: 0..255
}).

-type mpls_stack_entry() :: #mpls_stack_entry{} | #mpls_stack_entry_ext{}.

-record(srte_nai_node, {
    address :: inet:ip_address()
}).

-record(srte_nai_adjacency, {
    local_address :: inet:ip_address(),
    remote_address :: inet:ip_address()
}).

-record(srte_nai_unumbered_ipv4_adjacency, {
    local_node_id :: non_neg_integer(),
    local_interface_id :: non_neg_integer(),
    remote_node_id :: non_neg_integer(),
    remote_interface_id :: non_neg_integer()
}).

-record(srte_nai_linklocal_ipv6_adjacency, {
    local_address :: inet:ip6_address(),
    local_interface_id :: non_neg_integer(),
    remote_address :: inet:ip6_address(),
    remote_interface_id :: non_neg_integer()
}).

-type srte_nai() :: #srte_nai_node{}
                  | #srte_nai_adjacency{}
                  | #srte_nai_unumbered_ipv4_adjacency{}
                  | #srte_nai_linklocal_ipv6_adjacency{}.

-endif. % PCEP_CODEC_TE_INCLUDED