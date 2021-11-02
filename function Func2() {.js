function Func2() {
    var result = new Array();

    for(var i=0; i < 10; i++){
        result[i] = function(num) {
            console.log('num:'+num);
            return num;
        }(i);
    }

    return result;
}

console.log(Func2());