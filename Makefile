DOMAIN=localhost
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
PFDNLD_LINK_DIRECTORY=$(FS_ROOT_DIRECTORY)/pfdnld
PFDNLD_LINK_FILENAME=$(PFDNLD_LINK_DIRECTORY)/pfdnld-links.txt
PFDNLD_ROOT_DIRECTORY=$(VAR_DIR)/lib/$(DOMAIN)-pfdnld
PFDNLD_RESULT_FILENAME=$(PFDNLD_ROOT_DIRECTORY)/pfdnld.txt
PFDNLD_TMP_DIR=$(TMP_DIR)/pfdnld

MINIO=minio
MINIO_BIND_ADDRESS=127.0.0.1:9000
MINIO_BIND_CONSOLE_ADDRESS=127.0.0.1:9001

GOTIFY=gotify
GOTIFY_BIND_PORT=8080
GOTIFY_BIND_IP=127.0.0.1
GOTIFY_BIND_ADDRESS=$(GOTIFY_BIND_IP):$(GOTIFY_BIND_PORT)
GOTIFY_DIRECTORY=$(VAR_DIR)/lib/notifcations.$(DOMAIN)
GOTIFY_IMAGE_DIRECTORY=$(GOTIFY_DIRECTORY)/images
GOTIFY_DB_FILE=$(GOTIFY_DIRECTORY)/notifcations.db

BUILD_DIR=_build
REPLACE_VALUES=$(DOMAIN) $(INSTALL_ROOT_DIRECTORY)/ $(FS_ROOT_DIRECTORY)/ $(MINIO_BIND_ADDRESS) $(MINIO_BIND_CONSOLE_ADDRESS) $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)/ $(CADDY_LOG_DIRECTORY)/ $(CADDY_LOG_LEVEL) $(PFDNLD_ROOT_DIRECTORY)/ $(GOTIFY_BIND_ADDRESS)
ADD_NEWLINES_TO_CADDY_CONFIG_FILE=@ echo >> $(CADDY_BUILD_CONFIG_FILE)
ECHO_LINE_SEPERATOR=@ echo "--------------------------------------------------------------------------------"

ARIA2=aria2c

CADDY_CHECK:=$(shell command -v $(CADDY) 2> /dev/null)
ifndef CADDY_CHECK
 $(warning "Could not found caddy at '$(CADDY)'. For more info about installation see https://caddyserver.com")
endif

MINIO_CHECK:=$(shell command -v $(MINIO) 2> /dev/null)
ifndef MINIO_CHECK
 $(warning "Could not found minio at '$(MINIO)'. For more info about installation see https://min.io/")
endif

GOTIFY_CHECK:=$(shell command -v $(GOTIFY) 2> /dev/null)
ifndef GOTIFY_CHECK
 $(warning "Could not found gotify at '$(GOTIFY)'. For more info about installation see https://gotify.net/")
endif

HUGO_CHECK:=$(shell command -v $(HUGO) 2> /dev/null)
ifndef HUGO_CHECK
 $(error "Could not found hugo at '$(HUGO)'. For more info about installation see https://gohugo.io")
endif

ARIA2_CHECK:=$(shell command -v $(ARIA2) 2> /dev/null)
ifndef ARIA2_CHECK
 $(error "Could not found aria2 at '$(ARIA2)'. For more info about installation see https://aria2.github.io/")
endif


all: build install


build: build-caddy-config build-about-subdomain


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

	./tools/replace.sh subdomains/notifications/Caddyfile $(BUILD_DIR)/tmp/notifications.Caddyfile $(REPLACE_VALUES)
	cat $(BUILD_DIR)/tmp/notifications.Caddyfile >> $(CADDY_BUILD_CONFIG_FILE)
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
	mkdir -p $(PFDNLD_LINK_DIRECTORY)
	cp $(CADDY_BUILD_CONFIG_FILE) $(CADDY_CONFIG_FILE)
	touch $(PFDNLD_LINK_FILENAME)
	mkdir -p $(PFDNLD_ROOT_DIRECTORY)
	touch $(PFDNLD_RESULT_FILENAME)
	$(ECHO_LINE_SEPERATOR)
	mkdir -p $(GOTIFY_DIRECTORY)
	mkdir -p $(GOTIFY_IMAGE_DIRECTORY)
	@ tree $(INSTALL_ROOT_DIRECTORY) 2>/dev/null || true
	$(ECHO_LINE_SEPERATOR)
	@ echo "Run Caddy webserver:"
	@ echo "  $(CADDY) run -adapter caddyfile -config $(CADDY_CONFIG_FILE)"
	@ echo "Run Minio:"
	@ echo "  MINIO_ROOT_USER=<YOUR_USER> MINIO_ROOT_PASSWORD=<YOUR_PASSWORD> $(MINIO) server --address $(MINIO_BIND_ADDRESS) --console-address $(MINIO_BIND_CONSOLE_ADDRESS) $(FS_ROOT_DIRECTORY)"
	@ echo "Run Gotify:"
	@ echo "  GOTIFY_DEFAULTUSER_NAME=<YOUR_USER> GOTIFY_DEFAULTUSER_PASS=<YOUR_PASSWORD> GOTIFY_UPLOADEDIMAGESDIR=$(GOTIFY_IMAGE_DIRECTORY) GOTIFY_DATABASE_CONNECTION=$(GOTIFY_DB_FILE) GOTIFY_SERVER_PORT=$(GOTIFY_BIND_PORT) GOTIFY_SERVER_LISTENADDR=$(GOTIFY_BIND_IP) $(GOTIFY)"
	@ echo "Run PFDNLD:"
	@ echo "  $(PFDNLD) -H notifications.$(DOMAIN) -P 443 -p 15 --application-token <YOUR_APP_TOKEN> --application-id <YOUR_APP_ID_INTEGER> --client-token <YOUR_CLIENT_TOKEN> --tls --out-dir $(FS_ROOT_DIRECTORY) --tmp-dir $(PFDNLD_TMP_DIR) --markdown"
	$(ECHO_LINE_SEPERATOR)


clean:
	rm -rf _build


dist-clean: clean
	$(ECHO_LINE_SEPERATOR)
	@ echo "Run below commands:"
	@ echo "rm -rf $(PFDNLD)"
	@ echo "rm -rf $(PFDNLD_ROOT_DIRECTORY)"
	@ echo "rm -rf $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)"
	@ echo "rm -rf $(ABOUT_SUBDOMAIN_LOG_FILE)"
	@ echo "rm -rf $(PFDNLD_TMP_DIR)"
	@ echo "rm -rf $(GOTIFY_DIRECTORY)"
	$(ECHO_LINE_SEPERATOR)
	@ echo "WARNING: Check directories before running above commands or you maye remove your data permanently!"


caddy-format:
	caddy fmt -overwrite Caddyfile
	caddy fmt -overwrite subdomains/about/Caddyfile
	caddy fmt -overwrite subdomains/books/Caddyfile
	caddy fmt -overwrite subdomains/fs/Caddyfile
	caddy fmt -overwrite subdomains/music/Caddyfile
	caddy fmt -overwrite subdomains/public/Caddyfile
	caddy fmt -overwrite subdomains/videos/Caddyfile
