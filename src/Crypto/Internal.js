const crypto = require('crypto');
const generateRSAKeypair = require('generate-rsa-keypair');

exports._generateRSAKeypair = function (toPrivateKey, toPublicKey) {
  const keypair = generateRSAKeypair();
  return {
    private: toPrivateKey(keypair.private),
    public: toPublicKey(keypair.public),
  };
};

exports._rsaVerify = function (publicKey, signature, data) {
  const key = crypto.createPublicKey(publicKey);
  const verifier = crypto.createVerify('RSA-SHA256');

  verifier.update(data);

  if (verifier.verify(key, signature) === true) {
    return true;
  } else {
    return false;
  }
};

exports._rsaSign = function (privateKey, data) {
  const sign = crypto.createSign('RSA-SHA256');
  sign.update(data);
  sign.end();
  return sign.sign(privateKey, 'base64');
};
