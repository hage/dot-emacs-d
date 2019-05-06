.PHONY: all info-ja lsp-servers

all: info-ja lsp-servers


INFO_JA_URL=https://ayatakesi.github.io/emacs/26.1/emacs-ja.info
INFO_JA_PATH=~/.emacs.d/info

info-ja:
	mkdir -p $(INFO_JA_PATH)
	curl $(INFO_JA_URL) > $(INFO_JA_PATH)/emacs-ja.info


lsp-servers:
	cd lsp-servers && $(MAKE)

clean:
	find lsp-servers -mindepth 1 -maxdepth 1 -type d | xargs rm -rf
