DOMAIN=pouriya.net
CADDY_CONFIG_FILENAME=$(DOMAIN).Caddyfile
CADDY_CONFIG_FILE=$(BUILD_DIR)/etc/$(CADDY_CONFIG_FILENAME)
DEST_DIR=$(CURDIR)/dist
ROOT_DIRECTORY=$(CURDIR)/dist
FS_ROOT_DIRECTORY=$(ROOT_DIRECTORY)/fs
ABOUT_SUBDOMAIN_ROOT_DIRECTORY=$(ROOT_DIRECTORY)/var/lib/about.$(DOMAIN)
MINIO_BIND_ADDRESS=127.0.0.1:9000
MINIO_BIND_CONSOLE_ADDRESS=127.0.0.1:9001

BUILD_DIR=_build


ifeq (, $(shell which caddy))
 $(warning "Could not found caddy in PATH, consider installing Caddy Webserver from https://caddyserver.com")
endif

ifeq (, $(shell which minio))
 $(warning "Could not found minio in PATH, consider installing Minio Server from https://min.io/")
endif

ifeq (, $(shell which hugo))
 $(error "Could not found hugo in PATH, consider installing Hugo from https://gohugo.io")
endif


build-caddy-config: make-build-directory
	mkdir -p $(BUILD_DIR)/etc/ && mkdir -p $(BUILD_DIR)/tmp/ && rm -rf $(CADDY_CONFIG_FILE)
	
	./tools/replace.sh subdomains/about/Caddyfile $(BUILD_DIR)/tmp/about.Caddyfile $(DOMAIN) $(FS_ROOT_DIRECTORY)/ $(MINIO_BIND_ADDRESS) $(MINIO_BIND_CONSOLE_ADDRESS) $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)
	cat $(BUILD_DIR)/tmp/about.Caddyfile >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	
	./tools/replace.sh subdomains/books/Caddyfile $(BUILD_DIR)/tmp/books.Caddyfile $(DOMAIN) $(FS_ROOT_DIRECTORY)/ $(MINIO_BIND_ADDRESS) $(MINIO_BIND_CONSOLE_ADDRESS) $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)
	cat $(BUILD_DIR)/tmp/books.Caddyfile >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	
	./tools/replace.sh subdomains/fs/Caddyfile $(BUILD_DIR)/tmp/fs.Caddyfile $(DOMAIN) $(FS_ROOT_DIRECTORY)/ $(MINIO_BIND_ADDRESS) $(MINIO_BIND_CONSOLE_ADDRESS) $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)
	cat $(BUILD_DIR)/tmp/fs.Caddyfile >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	
	./tools/replace.sh subdomains/music/Caddyfile $(BUILD_DIR)/tmp/music.Caddyfile $(DOMAIN) $(FS_ROOT_DIRECTORY)/ $(MINIO_BIND_ADDRESS) $(MINIO_BIND_CONSOLE_ADDRESS) $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)
	cat $(BUILD_DIR)/tmp/music.Caddyfile >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	
	./tools/replace.sh subdomains/public/Caddyfile $(BUILD_DIR)/tmp/public.Caddyfile $(DOMAIN) $(FS_ROOT_DIRECTORY)/ $(MINIO_BIND_ADDRESS) $(MINIO_BIND_CONSOLE_ADDRESS) $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)
	cat $(BUILD_DIR)/tmp/public.Caddyfile >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	echo >> $(CADDY_CONFIG_FILE)
	
	./tools/replace.sh subdomains/videos/Caddyfile $(BUILD_DIR)/tmp/videos.Caddyfile $(DOMAIN) $(FS_ROOT_DIRECTORY)/ $(MINIO_BIND_ADDRESS) $(MINIO_BIND_CONSOLE_ADDRESS) $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)
	cat $(BUILD_DIR)/tmp/videos.Caddyfile >> $(CADDY_CONFIG_FILE)


make-build-directory:
	mkdir -p $(BUILD_DIR)
	rm -rf $(BUILD_DIR)/tmp


install:
	mkdir -p $(ROOT_DIRECTORY)
	mkdir -p $(FS_ROOT_DIRECTORY)
	mkdir -p $(ABOUT_SUBDOMAIN_ROOT_DIRECTORY)
	mkdir -p $(FS_ROOT_DIRECTORY)/books
	mkdir -p $(FS_ROOT_DIRECTORY)/music
	mkdir -p $(FS_ROOT_DIRECTORY)/public
	mkdir -p $(FS_ROOT_DIRECTORY)/videos
	mkdir -p $(DEST_DIR) && mkdir -p $(DEST_DIR)/etc && cp $(CADDY_CONFIG_FILE) $(DEST_DIR)/etc
	@ echo --------------------------------------------------------------------------------
	@ echo Run Caddy webserver: caddy run -config $(DEST_DIR)/etc/$(CADDY_CONFIG_FILENAME)
	@ echo "Run Minio: MINIO_ROOT_USER=<YOUR_USER> MINIO_ROOT_PASSWORD=<YOUR_PASSWORD> minio server --address $(MINIO_BIND_ADDRESS) --console-address $(MINIO_BIND_CONSOLE_ADDRESS) $(ROOT_DIRECTORY)"
