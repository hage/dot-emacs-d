.PHONY: elixir-server

elixir-server:
	[ -e elixir-ls ] || git clone https://github.com/elixir-lsp/elixir-ls.git
	cd elixir-ls && mix deps.get && mix elixir_ls.release
