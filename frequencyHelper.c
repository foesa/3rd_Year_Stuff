int frequencies [10]; //say we can pick between 1 and 10
int pickedNums[] = {1,3,4}; // numbers we've picked
for(i = 0;i < sizeof(pickedNums)/sizeof(pickedNums[0]);i++){ //loop to go through each number in pickedNums
  frequencies[pickedNums[i]-1] = frequencies[pickedNums[i]-1]++; //increment relevant index by subtracting by one since arrays start at 0
}
