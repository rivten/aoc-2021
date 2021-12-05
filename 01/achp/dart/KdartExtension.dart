import 'dart:math';

extension Kdart<T> on List<T> {
  List<List<T>> windowed(
      {int size = 1, int step = 1, bool partialWindow = false}) {
    List<List<T>> resultList = [];
    for (var i = 0; i < this.length; ++i) {
      if (i + size > this.length) {
        if (partialWindow) {
          resultList.add(this.sublist(i, min(i + size, this.length)));
        }
      } else {
        resultList.add(this.sublist(i, min(i + size, this.length)));
      }
      i += step - 1;
    }
    return resultList;
  }

  List<U> zipWithNext<U>({U transform(T a, T b)?}){
    List<U> result = [];
    for (var i = 0; i < this.length; ++i) {
      if((i+1)<this.length){
        if(transform != null){
          result.add(transform(this[i], this[i+1]));
        }else{
          result.add([this[i], this[i+1]] as U);
        }
      }
    }
    return result;
  }

  int count(bool callback(T item)){
    return this.fold(0, (prev, item) => callback(item)? prev + 1: prev);
  }

  void forEachIndexed(void callback(T item, int index)){
    for (var i = 0; i < this.length; ++i) {
      callback(this[i], i);
    }
  }
}

extension KdartInt<T extends int> on List<T> {
  int sum(){
    return this.fold(0, (int prev, int curr) => prev + curr);
  }
}

