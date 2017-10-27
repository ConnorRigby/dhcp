defmodule DHCPServer.Lib do
  @moduledoc "Defines for DHCP Packets."
  @doc false
  defmacro __using__(_) do
    quote do
      @dhcp_server_port 67
      @dhcp_client_port 68
      @inaddr_any {0,0,0,0}
      @inaddr_broadcast {255, 255, 255, 255}
      @bootp_broadcast 0x8000
      @dhcp_options_cookie [99, 130, 83, 99]
      @bootrequest 1
      @bootreply 2
      @htype_ether 1
      @htype_ieee802 6
      @htype_fddi 8

      @dho_pad 0
      @dho_subnet_mask 1
      @dho_time_offset 2
      @dho_routers 3
      @dho_time_servers 4
      @dho_name_servers 5
      @dho_domain_name_servers 6
      @dho_log_servers 7
      @dho_cookie_servers 8
      @dho_lpr_servers 9
      @dho_impress_servers 10
      @dho_resource_location_servers 11
      @dho_host_name 12
      @dho_boot_size 13
      @dho_merit_dump 14
      @dho_domain_name 15
      @dho_swap_server 16
      @dho_root_path 17
      @dho_extensions_path 18
      @dho_ip_forwarding 19
      @dho_non_local_source_routing 20
      @dho_policy_filter 21
      @dho_max_dgram_reassembly 22
      @dho_default_ip_ttl 23
      @dho_path_mtu_aging_timeout 24
      @dho_path_mtu_plateau_table 25
      @dho_interface_mtu 26
      @dho_all_subnets_local 27
      @dho_broadcast_address 28
      @dho_perform_mask_discovery 29
      @dho_mask_supplier 30
      @dho_router_discovery 31
      @dho_router_solicitation_address 32
      @dho_static_routes 33
      @dho_trailer_encapsulation 34
      @dho_arp_cache_timeout 35
      @dho_ieee802_3_encapsulation 36
      @dho_default_tcp_ttl 37
      @dho_tcp_keepalive_interval 38
      @dho_tcp_keepalive_garbage 39
      @dho_nis_domain 40
      @dho_nis_servers 41
      @dho_ntp_servers 42
      @dho_vendor_encapsulated_options 43
      @dho_netbios_name_servers 44
      @dho_netbios_dd_servers 45
      @dho_netbios_node_type 46
      @dho_netbios_scope 47
      @dho_font_servers 48
      @dho_x_display_managers 49
      @dho_dhcp_requested_address 50
      @dho_dhcp_lease_time 51
      @dho_dhcp_option_overload 52
      @dho_dhcp_message_type 53
      @dho_dhcp_server_identifier 54
      @dho_dhcp_parameter_request_list 55
      @dho_dhcp_message 56
      @dho_dhcp_max_message_size 57
      @dho_dhcp_renewal_time 58
      @dho_dhcp_rebinding_time 59
      @dho_vendor_class_identifier 60
      @dho_dhcp_client_identifier 61
      @dho_nwip_domain_name 62
      @dho_nwip_suboptions 63
      @dho_nis_plus_domain 64
      @dho_nis_plus_servers 65
      @dho_tftp_server_name 66
      @dho_bootfile_name 67
      @dho_mobile_ip_home_agents 68
      @dho_smtp_servers 69
      @dho_pop3_servers 70
      @dho_nntp_servers 71
      @dho_www_servers 72
      @dho_finger_servers 73
      @dho_irc_servers 74
      @dho_streettalk_servers 75
      @dho_stda_servers 76
      @dho_user_class 77
      @dho_fqdn 81
      @dho_dhcp_agent_options 82
      @dho_nds_servers 85
      @dho_nds_tree_name 86
      @dho_nds_context 87
      @dho_uap 98
      @dho_auto_configure 116
      @dho_name_service_search 117
      @dho_subnet_selection 118
      @dho_tftp_server_address 150
      @dho_end 255

      @dhcpdiscover 1
      @dhcpoffer 2
      @dhcprequest 3
      @dhcpdecline 4
      @dhcpack 5
      @dhcpnak 6
      @dhcprelease 7
      @dhcpinform 8

      @rai_circuit_id 1
      @rai_remote_id 2
      @rai_agent_id 3
      @fqdn_no_client_update 1
      @fqdn_server_update 2
      @fqdn_encoded 3
      @fqdn_rcode1 4
      @fqdn_rcode2 5
      @fqdn_hostname 6
      @fqdn_domainname 7
      @fqdn_fqdn 8
      @fqdn_suboption_count 8

      def type(@dho_subnet_mask), do: :ip
      def type(@dho_time_offset), do: :integer
      def type(@dho_routers), do: :iplist
      def type(@dho_time_servers), do: :iplist
      def type(@dho_name_servers), do: :iplist
      def type(@dho_domain_name_servers), do: :iplist
      def type(@dho_log_servers), do: :iplist
      def type(@dho_cookie_servers), do: :iplist
      def type(@dho_lpr_servers), do: :iplist
      def type(@dho_impress_servers), do: :iplist
      def type(@dho_resource_location_servers), do: :iplist
      def type(@dho_host_name), do: :string
      def type(@dho_boot_size), do: :short
      def type(@dho_merit_dump), do: :string
      def type(@dho_domain_name), do: :string
      def type(@dho_swap_server), do: :ip
      def type(@dho_root_path), do: :string
      def type(@dho_extensions_path), do: :string
      def type(@dho_ip_forwarding), do: :byte
      def type(@dho_non_local_source_routing), do: :byte
      def type(@dho_policy_filter), do: :iplist
      def type(@dho_max_dgram_reassembly), do: :short
      def type(@dho_default_ip_ttl), do: :byte
      def type(@dho_path_mtu_aging_timeout), do: :integer
      def type(@dho_path_mtu_plateau_table), do: :integer
      def type(@dho_interface_mtu), do: :short
      def type(@dho_all_subnets_local), do: :byte
      def type(@dho_broadcast_address), do: :ip
      def type(@dho_perform_mask_discovery), do: :byte
      def type(@dho_mask_supplier), do: :byte
      def type(@dho_router_discovery), do: :byte
      def type(@dho_router_solicitation_address), do: :ip
      def type(@dho_static_routes), do: :iplist
      def type(@dho_trailer_encapsulation), do: :byte
      def type(@dho_arp_cache_timeout), do: :integer
      def type(@dho_ieee802_3_encapsulation), do: :byte
      def type(@dho_default_tcp_ttl), do: :byte
      def type(@dho_tcp_keepalive_interval), do: :integer
      def type(@dho_tcp_keepalive_garbage), do: :byte
      def type(@dho_nis_domain), do: :string
      def type(@dho_nis_servers), do: :iplist
      def type(@dho_ntp_servers), do: :iplist
      def type(@dho_tftp_server_name), do: :string
      def type(@dho_bootfile_name), do: :string
      def type(@dho_vendor_encapsulated_options), do: :vendor
      def type(@dho_netbios_name_servers), do: :iplist
      def type(@dho_netbios_dd_servers), do: :iplist
      def type(@dho_netbios_node_type), do: :byte
      def type(@dho_netbios_scope), do: :string
      def type(@dho_font_servers), do: :iplist
      def type(@dho_x_display_managers), do: :iplist
      def type(@dho_dhcp_requested_address), do: :ip
      def type(@dho_dhcp_lease_time), do: :integer
      def type(@dho_dhcp_option_overload), do: :byte
      def type(@dho_dhcp_message_type), do: :byte
      def type(@dho_dhcp_server_identifier), do: :ip
      def type(@dho_dhcp_parameter_request_list), do: :string
      def type(@dho_dhcp_message), do: :string
      def type(@dho_dhcp_max_message_size), do: :short
      def type(@dho_dhcp_renewal_time), do: :integer
      def type(@dho_dhcp_rebinding_time), do: :integer
      def type(@dho_vendor_class_identifier), do: :string
      def type(@dho_dhcp_client_identifier), do: :string
      def type(@dho_nwip_domain_name), do: :string
      def type(@dho_nis_plus_domain), do: :string
      def type(@dho_nis_plus_servers), do: :iplist
      def type(@dho_mobile_ip_home_agents), do: :iplist
      def type(@dho_smtp_servers), do: :iplist
      def type(@dho_pop3_servers), do: :iplist
      def type(@dho_www_servers), do: :iplist
      def type(@dho_finger_servers), do: :iplist
      def type(@dho_irc_servers), do: :iplist
      def type(@dho_streettalk_servers), do: :iplist
      def type(@dho_stda_servers), do: :iplist
      def type(@dho_user_class), do: :string
      def type(@dho_fqdn), do: :string
      def type(@dho_dhcp_agent_options), do: :string
      def type(@dho_nds_servers), do: :iplist
      def type(@dho_nds_tree_name), do: :string
      def type(@dho_nds_context), do: :string
      def type(@dho_uap), do: :string
      def type(@dho_auto_configure), do: :byte
      def type(@dho_name_service_search), do: :shortlist
      def type(@dho_subnet_selection), do: :ip
      def type(@dho_tftp_server_address), do: :ip
      def type(_), do: :unknown

    end
  end
end
