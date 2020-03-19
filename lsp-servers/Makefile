.PHONY: all elixir-server php-server

all: elixir-server php-server

elixir-server:
	[ -e elixir-ls ] || git clone https://github.com/elixir-lsp/elixir-ls.git
	cd elixir-ls && git pull && mix deps.get deps.update elixir_ls.release

php-server:
	cd php-ls && make

# for ruby
#   gem install solargraph

# for javascript
#   npm i -g javascript-typescript-langserver
#   npm i -g typescript-language-server; npm i -g typescript
