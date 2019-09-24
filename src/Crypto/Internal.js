const generateRSAKeypair = require('generate-rsa-keypair');

exports._generateRSAKeypair = function (privateKey) {
  return function (publicKey) {
    return function () {
      const keypair = generateRSAKeypair();
      return { 
        private: privateKey(keypair.private),
        public: publicKey(keypair.public),
      };
    };
  };
};
