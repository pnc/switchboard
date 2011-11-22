openssl pkcs12 -clcerts -nokeys -out apns-dev-cert.pem -in apns-dev-cert.p12
openssl pkcs12 -nocerts -out apns-dev-key.pem -in apns-dev-key.p12
openssl rsa -in apns-dev-key.pem -out apns-dev-key-noenc.pem
cat apns-dev-cert.pem apns-dev-key-noenc.pem > apns-dev.pem
