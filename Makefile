DOMAIN=pouriya.net
PREFIX=$(CURDIR)/_test
INSTALL_ROOT_DIRECTORY=$(PREFIX)
FS_ROOT_DIRECTORY=$(INSTALL_ROOT_DIRECTORY)/srv/minio
ABOUT_SUBDOMAIN_ROOT_DIRECTORY=$(INSTALL_ROOT_DIRECTORY)/var/lib/about.$(DOMAIN)

CADDY_CONFIG_FILENAME=$(DOMAIN).Caddyfile
CADDY_CONFIG_FILE=$(BUILD_DIR)/etc/$(CADDY_CONFIG_FILENAME)
CADDY_LOG_LEVEL=info
CADDY_LOG_DIRECTORY=$(INSTALL_ROOT_DIRECTORY)/var/log

MINIO_BIND_ADDRESS=127.0.0.1:9000
MINIO_BIND_CONSOLE_ADDRESS=127.0.0.1:9001

BUILD_DIR=_build
REPLACE_VALUES=$(DOMAIN) $(INSTALL_ROOT_DIRECTORY)/ $(FS_ROOT_DIRECTORY)/ $(MINIO_BIND_ADDRESS) $(MINIO_BIND_CONSOLE_ADDRESS) $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)/ $(CADDY_LOG_DIRECTORY)/ $(CADDY_LOG_LEVEL)
ADD_NEWLINES_TO_CADDY_CONFIG_FILE=@ echo >> $(CADDY_CONFIG_FILE)
ECHO_LINE_SEPERATOR=@ echo "--------------------------------------------------------------------------------"


ifeq (, $(shell which caddy))
 $(warning "Could not found caddy in PATH, consider installing Caddy Webserver from https://caddyserver.com")
endif

ifeq (, $(shell which minio))
 $(warning "Could not found minio in PATH, consider installing Minio Server from https://min.io/")
endif

ifeq (, $(shell which hugo))
 $(error "Could not found hugo in PATH, consider installing Hugo from https://gohugo.io")
endif


all: build install


build: build-caddy-config build-about-subdomain


localhost-all:
	$(MAKE) DOMAIN=localhost all


localhost-build:
	$(MAKE) DOMAIN=localhost build


build-caddy-config: ensure-build-directory
	mkdir -p $(BUILD_DIR)/etc/ && mkdir -p $(BUILD_DIR)/tmp/

	./tools/replace.sh ./Caddyfile $(BUILD_DIR)/tmp/Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/Caddyfile >> $(CADDY_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/about/Caddyfile $(BUILD_DIR)/tmp/about.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/about.Caddyfile >> $(CADDY_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/books/Caddyfile $(BUILD_DIR)/tmp/books.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/books.Caddyfile >> $(CADDY_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/fs/Caddyfile $(BUILD_DIR)/tmp/fs.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/fs.Caddyfile >> $(CADDY_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/music/Caddyfile $(BUILD_DIR)/tmp/music.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/music.Caddyfile >> $(CADDY_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/public/Caddyfile $(BUILD_DIR)/tmp/public.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/public.Caddyfile >> $(CADDY_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/videos/Caddyfile $(BUILD_DIR)/tmp/videos.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/videos.Caddyfile >> $(CADDY_CONFIG_FILE)


build-about-subdomain:
	git submodule update --init
	cd subdomains/about && hugo --destination $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY) --baseURL https://about.$(DOMAIN) --minify


ensure-build-directory: clean
	mkdir -p $(BUILD_DIR) && rm -rf $(BUILD_DIR)/tmp && rm -rf $(BUILD_DIR)/etc


install:
	mkdir -p $(INSTALL_ROOT_DIRECTORY)
	mkdir -p $(INSTALL_ROOT_DIRECTORY)/bin
	cp deps/pfdnld/pfdnld.py $(INSTALL_ROOT_DIRECTORY)/bin/pfdnld
	chmod a+x $(INSTALL_ROOT_DIRECTORY)/bin/pfdnld
	mkdir -p $(FS_ROOT_DIRECTORY)
	mkdir -p $(CADDY_LOG_DIRECTORY)
	mkdir -p $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)
	touch $(CADDY_LOG_DIRECTORY)/about.$(DOMAIN).log
	mkdir -p $(FS_ROOT_DIRECTORY)/books
	mkdir -p $(FS_ROOT_DIRECTORY)/music
	mkdir -p $(FS_ROOT_DIRECTORY)/public
	mkdir -p $(FS_ROOT_DIRECTORY)/videos
	mkdir -p $(FS_ROOT_DIRECTORY)/videos/movies
	mkdir -p $(FS_ROOT_DIRECTORY)/videos/series
	mkdir -p $(INSTALL_ROOT_DIRECTORY)/etc && cp $(CADDY_CONFIG_FILE) $(INSTALL_ROOT_DIRECTORY)/etc
	$(ECHO_LINE_SEPERATOR)
	@ tree $(INSTALL_ROOT_DIRECTORY) 2>/dev/null || true
	$(ECHO_LINE_SEPERATOR)
	@ echo "Run Caddy webserver:"
	@ echo "  caddy run -adapter caddyfile -config $(INSTALL_ROOT_DIRECTORY)/etc/$(CADDY_CONFIG_FILENAME)"
	@ echo "Run Minio:"
	@ echo "  MINIO_ROOT_USER=<YOUR_USER> MINIO_ROOT_PASSWORD=<YOUR_PASSWORD> minio server --address $(MINIO_BIND_ADDRESS) --console-address $(MINIO_BIND_CONSOLE_ADDRESS) $(FS_ROOT_DIRECTORY)"
	$(ECHO_LINE_SEPERATOR)


clean:
	rm -rf _build


dist-clean: clean
	$(ECHO_LINE_SEPERATOR)
	@ echo "Run below commands:"
	@ echo "rm -rf $(INSTALL_ROOT_DIRECTORY)/bin/pfdnld"
	@ echo "rm -rf $(FS_ROOT_DIRECTORY)"
	@ echo "rm -rf $(CADDY_LOG_DIRECTORY)/about.$(DOMAIN).log"
	@ echo "rm -rf $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)"
	@ echo "rm -rf $(INSTALL_ROOT_DIRECTORY)/etc/$(CADDY_CONFIG_FILENAME)"
	$(ECHO_LINE_SEPERATOR)
	@ echo "WARNING: Check directories before running above commands or you maye remove your data permanently!"


localhost-dist-clean:
	$(MAKE) DOMAIN=localhost dist-clean


caddy-format:
	caddy fmt -overwrite Caddyfile
	caddy fmt -overwrite subdomains/about/Caddyfile
	caddy fmt -overwrite subdomains/books/Caddyfile
	caddy fmt -overwrite subdomains/fs/Caddyfile
	caddy fmt -overwrite subdomains/music/Caddyfile
	caddy fmt -overwrite subdomains/public/Caddyfile
	caddy fmt -overwrite subdomains/videos/Caddyfile
