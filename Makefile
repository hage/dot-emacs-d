.PHONY: info-ja

INFO_JA_URL=https://ayatakesi.github.io/emacs/26.1/emacs-ja.info
INFO_JA_PATH=~/.emacs.d/info

info-ja:
	mkdir -p $(INFO_JA_PATH)
	curl $(INFO_JA_URL) > $(INFO_JA_PATH)/emacs-ja.info
