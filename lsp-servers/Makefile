.PHONY: all elixir-server php-server

all: elixir-server php-server

elixir-server:
	[ -e elixir-ls ] || git clone https://github.com/elixir-lsp/elixir-ls.git
	cd elixir-ls && git pull && mix deps.get deps.update elixir_ls.release

php-server:
	cd php-ls && make
