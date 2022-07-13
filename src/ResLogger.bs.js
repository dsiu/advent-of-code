// Generated by ReScript, PLEASE EDIT WITH CARE


function prependDate(message) {
  var now = new Date();
  var hours = now.getHours().toString().padStart(2, "0");
  var minutes = now.getMinutes().toString().padStart(2, "0");
  var seconds = now.getSeconds().toString().padStart(2, "0");
  return hours + ":" + minutes + ":" + seconds + " " + message;
}

function log(level, msg) {
  var msg$1 = prependDate(msg);
  switch (level) {
    case /* Debug */0 :
        if (process.env.NODE_ENV === "development") {
          console.log(msg$1);
          return ;
        } else {
          return ;
        }
    case /* Info */1 :
        console.info(msg$1);
        return ;
    case /* Warn */2 :
        console.warn(msg$1);
        return ;
    case /* Error */3 :
        console.error(msg$1);
        return ;
    
  }
}

function log2(level, msg, obj) {
  var msg$1 = prependDate(msg);
  switch (level) {
    case /* Debug */0 :
        if (process.env.NODE_ENV === "development") {
          console.log(msg$1, obj);
          return ;
        } else {
          return ;
        }
    case /* Info */1 :
        console.info(msg$1, obj);
        return ;
    case /* Warn */2 :
        console.warn(msg$1, obj);
        return ;
    case /* Error */3 :
        console.error(msg$1, obj);
        return ;
    
  }
}

var DefaultImpl = {
  log: log,
  log2: log2
};

var loggerImpl = {
  contents: DefaultImpl
};

function setLoggerImpl(impl) {
  loggerImpl.contents = impl;
  
}

function make(moduleName) {
  var prefix = "[" + moduleName + "] ";
  var debug = function (message) {
    var I = loggerImpl.contents;
    return I.log(/* Debug */0, prefix + message);
  };
  var info = function (message) {
    var I = loggerImpl.contents;
    return I.log(/* Info */1, prefix + message);
  };
  var warn = function (message) {
    var I = loggerImpl.contents;
    return I.log(/* Warn */2, prefix + message);
  };
  var error = function (message) {
    var I = loggerImpl.contents;
    return I.log(/* Error */3, prefix + message);
  };
  var debug2 = function (message, obj) {
    var I = loggerImpl.contents;
    return I.log2(/* Debug */0, prefix + message, obj);
  };
  var info2 = function (message, obj) {
    var I = loggerImpl.contents;
    return I.log2(/* Info */1, prefix + message, obj);
  };
  var warn2 = function (message, obj) {
    var I = loggerImpl.contents;
    return I.log2(/* Warn */2, prefix + message, obj);
  };
  var error2 = function (message, obj) {
    var I = loggerImpl.contents;
    return I.log2(/* Error */3, prefix + message, obj);
  };
  return {
          debug: debug,
          info: info,
          warn: warn,
          error: error,
          debug2: debug2,
          info2: info2,
          warn2: warn2,
          error2: error2
        };
}

export {
  DefaultImpl ,
  loggerImpl ,
  setLoggerImpl ,
  make ,
  
}
/* No side effect */
