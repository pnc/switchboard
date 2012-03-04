# To generate apns-dev-cert.p12, go into Keychain Access, click "My Certificates", then
# find your APNS certificate and expand it. Select both items, export, and choose p12.
openssl pkcs12 -clcerts -nokeys -out apns-dev-cert.pem -in apns-dev-cert.p12
openssl pkcs12 -nocerts -out apns-dev-key.pem -in apns-dev-cert.p12
openssl rsa -in apns-dev-key.pem -out apns-dev-key-noenc.pem
cat apns-dev-cert.pem apns-dev-key-noenc.pem > apns-dev.pem
