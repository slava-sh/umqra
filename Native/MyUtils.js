Elm.Native = Elm.Native || {};
Elm.Native.MyUtils = {};
Elm.Native.MyUtils.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.MyUtils = localRuntime.Native.MyUtils || {};
    if (localRuntime.Native.MyUtils.values) {
        return localRuntime.Native.MyUtils.values;
    }

    function readSignal(signal) {
        return signal.value;
    }

    return localRuntime.Native.MyUtils.values = {
        readSignal: readSignal
    };
};
