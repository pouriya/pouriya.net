DOMAIN=pouriya.net
PREFIX=$(CURDIR)/_test
INSTALL_ROOT_DIRECTORY=$(PREFIX)
ETC_DIR=$(INSTALL_ROOT_DIRECTORY)/etc
BIN_DIR=$(INSTALL_ROOT_DIRECTORY)/bin
VAR_DIR=$(INSTALL_ROOT_DIRECTORY)/var
TMP_DIR=$(INSTALL_ROOT_DIRECTORY)/tmp
FS_ROOT_DIRECTORY=$(INSTALL_ROOT_DIRECTORY)/srv/minio
ABOUT_SUBDOMAIN_ROOT_DIRECTORY=$(VAR_DIR)/lib/about.$(DOMAIN)
ABOUT_SUBDOMAIN_LOG_FILE=$(VAR_DIR)/log/about.$(DOMAIN).log

HUGO=hugo

CADDY=caddy
CADDY_CONFIG_FILENAME=$(DOMAIN).Caddyfile
CADDY_BUILD_CONFIG_FILE=$(BUILD_DIR)/etc/$(CADDY_CONFIG_FILENAME)
CADDY_CONFIG_FILE=$(ETC_DIR)/$(CADDY_CONFIG_FILENAME)
CADDY_LOG_DIRECTORY=$(VAR_DIR)/log
CADDY_LOG_LEVEL=info

PFDNLD_DEPS=deps/pfdnld/pfdnld.py
PFDNLD=$(BIN_DIR)/pfdnld
PFDNLD_LINK_FILENAME=$(ETC_DIR)/pfdnld.links
PFDNLD_ROOT_DIRECTORY=$(VAR_DIR)/lib/$(DOMAIN)-pfdnld
PFDNLD_RESULT_FILENAME=$(PFDNLD_ROOT_DIRECTORY)/pfdnld.txt
PFDNLD_TMP_DIR=$(TMP_DIR)/pfdnld

MINIO=minio
MINIO_BIND_ADDRESS=127.0.0.1:9000
MINIO_BIND_CONSOLE_ADDRESS=127.0.0.1:9001

BUILD_DIR=_build
REPLACE_VALUES=$(DOMAIN) $(INSTALL_ROOT_DIRECTORY)/ $(FS_ROOT_DIRECTORY)/ $(MINIO_BIND_ADDRESS) $(MINIO_BIND_CONSOLE_ADDRESS) $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)/ $(CADDY_LOG_DIRECTORY)/ $(CADDY_LOG_LEVEL) $(PFDNLD_ROOT_DIRECTORY)/
ADD_NEWLINES_TO_CADDY_CONFIG_FILE=@ echo >> $(CADDY_BUILD_CONFIG_FILE)
ECHO_LINE_SEPERATOR=@ echo "--------------------------------------------------------------------------------"

CADDY_CHECK:=$(shell command -v $(CADDY) 2> /dev/null)
ifndef CADDY_CHECK
 $(warning "Could not found caddy at '$(CADDY)', consider installing Caddy Webserver from https://caddyserver.com")
endif

MINIO_CHECK:=$(shell command -v $(MINIO) 2> /dev/null)
ifndef MINIO_CHECK
 $(warning "Could not found minio at '$(MINIO)', consider installing Minio Server from https://min.io/")
endif

HUGO_CHECK:=$(shell command -v $(HUGO) 2> /dev/null)
ifndef HUGO_CHECK
 $(error "Could not found hugo at '$(HUGO)', consider installing Hugo from https://gohugo.io")
endif


all: build install


localhost-all:
	$(MAKE) DOMAIN=localhost all


build: build-caddy-config build-about-subdomain


localhost-build:
	$(MAKE) DOMAIN=localhost build


build-caddy-config: ensure-build-directory
	./tools/replace.sh ./Caddyfile $(BUILD_DIR)/tmp/Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/Caddyfile >> $(CADDY_BUILD_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/about/Caddyfile $(BUILD_DIR)/tmp/about.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/about.Caddyfile >> $(CADDY_BUILD_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/books/Caddyfile $(BUILD_DIR)/tmp/books.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/books.Caddyfile >> $(CADDY_BUILD_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/fs/Caddyfile $(BUILD_DIR)/tmp/fs.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/fs.Caddyfile >> $(CADDY_BUILD_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/music/Caddyfile $(BUILD_DIR)/tmp/music.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/music.Caddyfile >> $(CADDY_BUILD_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/public/Caddyfile $(BUILD_DIR)/tmp/public.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/public.Caddyfile >> $(CADDY_BUILD_CONFIG_FILE)
	$(ADD_NEWLINES_TO_CADDY_CONFIG_FILE)

	./tools/replace.sh subdomains/videos/Caddyfile $(BUILD_DIR)/tmp/videos.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/videos.Caddyfile >> $(CADDY_BUILD_CONFIG_FILE)


build-about-subdomain:
	cd subdomains/about && $(HUGO) --destination $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY) --baseURL https://about.$(DOMAIN) --minify


ensure-build-directory: clean
	mkdir -p $(BUILD_DIR)
	rm -rf $(BUILD_DIR)/tmp && mkdir -p $(BUILD_DIR)/tmp
	rm -rf $(BUILD_DIR)/etc && mkdir -p $(BUILD_DIR)/etc


install:
	mkdir -p $(INSTALL_ROOT_DIRECTORY)
	mkdir -p $(BIN_DIR)
	mkdir -p $(ETC_DIR)
	mkdir -p $(VAR_DIR)
	mkdir -p $(TMP_DIR)
	cp $(PFDNLD_DEPS) $(PFDNLD) && chmod a+x $(PFDNLD)
	mkdir -p $(PFDNLD_TMP_DIR)
	mkdir -p $(FS_ROOT_DIRECTORY)
	mkdir -p $(CADDY_LOG_DIRECTORY)
	mkdir -p $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)
	touch $(ABOUT_SUBDOMAIN_LOG_FILE)
	mkdir -p $(FS_ROOT_DIRECTORY)/books
	mkdir -p $(FS_ROOT_DIRECTORY)/music
	mkdir -p $(FS_ROOT_DIRECTORY)/public
	mkdir -p $(FS_ROOT_DIRECTORY)/videos
	mkdir -p $(FS_ROOT_DIRECTORY)/videos/movies
	mkdir -p $(FS_ROOT_DIRECTORY)/videos/series
	cp $(CADDY_BUILD_CONFIG_FILE) $(CADDY_CONFIG_FILE)
	touch $(PFDNLD_LINK_FILENAME)
	mkdir -p $(PFDNLD_ROOT_DIRECTORY)
	touch $(PFDNLD_RESULT_FILENAME)
	$(ECHO_LINE_SEPERATOR)
	@ tree $(INSTALL_ROOT_DIRECTORY) 2>/dev/null || true
	$(ECHO_LINE_SEPERATOR)
	@ echo "Run Caddy webserver:"
	@ echo "  $(CADDY) run -adapter caddyfile -config $(CADDY_CONFIG_FILE)"
	@ echo "Run Minio:"
	@ echo "  MINIO_ROOT_USER=<YOUR_USER> MINIO_ROOT_PASSWORD=<YOUR_PASSWORD> $(MINIO) server --address $(MINIO_BIND_ADDRESS) --console-address $(MINIO_BIND_CONSOLE_ADDRESS) $(FS_ROOT_DIRECTORY)"
	@ echo "Run PFDNLD:"
	@ echo "  $(PFDNLD) -l $(PFDNLD_LINK_FILENAME) -r $(PFDNLD_RESULT_FILENAME) --tmp-dir $(PFDNLD_TMP_DIR) --out-dir $(FS_ROOT_DIRECTORY) -p 30"
	$(ECHO_LINE_SEPERATOR)


clean:
	rm -rf _build


dist-clean: clean
	$(ECHO_LINE_SEPERATOR)
	@ echo "Run below commands:"
	@ echo "rm -rf $(PFDNLD)"
	@ echo "rm -rf $(FS_ROOT_DIRECTORY)"
	@ echo "rm -rf $(PFDNLD_ROOT_DIRECTORY)"
	@ echo "rm -rf $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)"
	@ echo "rm -rf $(ABOUT_SUBDOMAIN_LOG_FILE)"
	@ echo "rm -rf $(CADDY_CONFIG_FILE)"
	@ echo "rm -rf $(PFDNLD_TMP_DIR)"
	@ echo "rm -rf $(PFDNLD_LINK_FILENAME)"
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
