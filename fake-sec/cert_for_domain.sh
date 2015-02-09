
WHICH_SITE=$1

openssl genrsa  -out $WHICH_SITE-privkey.pem 2048
openssl req     -new -key $WHICH_SITE-privkey.pem -out $WHICH_SITE-cert.csr -subj "/commonName=$WHICH_SITE" -batch -config ca/openssl.cnf
openssl ca      -in $WHICH_SITE-cert.csr -out $WHICH_SITE-cert.pem -keyfile ca/private/cakey.pem -batch -cert ca/cacert.pem -config ca/openssl.cnf
