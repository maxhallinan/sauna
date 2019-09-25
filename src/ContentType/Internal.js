const contentType = require('content-type');

 exports._parse = function (eb, cb, s) {
   try {
     const ct = contentType.parse(s);
     return cb({ 
       mimeType: ct.type, 
       parameters: ct.parameters ? ct.parameters : {} 
     });
   } catch (error) {
     return eb(error.message);
   }
 };

