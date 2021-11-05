#! /bin/sh

set -e

domain="s&{DOMAIN}&$3&g"
root_directory="s&{ROOT_DIRECTORY}&$4&g"
minio_bind_address="s&{MINIO_BIND_ADDRESS}&$5&g"
minio_bind_console_address="s&{MINIO_BIND_CONSOLE_ADDRESS}&$6&g"
about_subdomain_root_directory="s&{ABOUT_SUBDOMAIN_ROOT_DIRECTORY}&$7&g"

rm -f $2
cp $1 $2
sed -i ${domain} $2
sed -i ${root_directory} $2
sed -i ${minio_bind_address} $2
sed -i ${minio_bind_console_address} $2
sed -i ${about_subdomain_root_directory} $2
