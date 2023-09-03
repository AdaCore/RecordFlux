IANA_REGISTRIES_DIR := iana_registries

.PHONY: generate_iana generate_iana_protocol_numbers generate_iana_tls_parameters generate_iana_tls_extensiontype_values generate_iana_bootp_dhcp_parameters generate_iana_arp_parameters

generate_iana: generate_iana_protocol_numbers generate_iana_tls_parameters generate_iana_tls_extensiontype_values generate_iana_bootp_dhcp_parameters generate_iana_arp_parameters

generate_iana_protocol_numbers:
	rflx convert iana $(IANA_REGISTRIES_DIR)/protocol-numbers.xml -a

generate_iana_tls_parameters:
	rflx convert iana $(IANA_REGISTRIES_DIR)/tls-parameters.xml -a

generate_iana_tls_extensiontype_values:
	rflx convert iana $(IANA_REGISTRIES_DIR)/tls-extensiontype-values.xml -a

generate_iana_bootp_dhcp_parameters:
	rflx convert iana $(IANA_REGISTRIES_DIR)/bootp-dhcp-parameters.xml

generate_iana_arp_parameters:
	rflx convert iana $(IANA_REGISTRIES_DIR)/arp-parameters.xml
