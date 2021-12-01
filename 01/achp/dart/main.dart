import 'dart:io';

List<U> mapIndexed<T, U>(List<T> arr, U callback(T e, int i)){
  List<U> arrayCopy = [];
  for(var i = 0; i < arr.length; i++ ) {
    arrayCopy.add(callback(arr[i], i));
  }
  return arrayCopy;
}

void main() async {
  var content = await File('./input.txt').readAsLines();

  var zippedLines = mapIndexed(content, (String e, i) {
    if(i==0) return [0, 0];
    else return [int.tryParse(content[i-1]), int.tryParse(e) ?? 0];
  });

  var filteredZippedLine = zippedLines.where((element) => (element[0] ?? 0) < (element[1]??0));
  print(filteredZippedLine.length);
}
