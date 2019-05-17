.PHONY: php-server php-composer

php-server: php-composer
	composer require felixfbecker/language-server
	composer run-script --working-dir=vendor/felixfbecker/language-server parse-stubs

php-composer:
	[ -e ~/bin/composer ] || (cd ~/bin && curl 'https://getcomposer.org/installer' | php && mv composer.phar composer)
