.PHONY: all info-ja lsp-servers

init.elc: init.el
	emacs --batch -f batch-byte-compile init.el

all: info-ja lsp-servers


INFO_JA_MAN_URL=https://ayatakesi.github.io/emacs/27.2/emacs-ja.info
INFO_JA_ELISP_URL=https://ayatakesi.github.io/lispref/27.2/elisp-ja.info
INFO_JA_PATH=~/.emacs.d/info

info-ja:
	mkdir -p $(INFO_JA_PATH)
	curl $(INFO_JA_MAN_URL) > $(INFO_JA_PATH)/emacs-ja.info
	curl $(INFO_JA_ELISP_URL) > $(INFO_JA_PATH)/elisp-ja.info


lsp-servers:
	cd lsp-servers && $(MAKE)

clean:
	find lsp-servers -mindepth 1 -maxdepth 1 -type d | xargs rm -rf
