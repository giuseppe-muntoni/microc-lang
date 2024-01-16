void printb(bool val)
{
  if(val)
    print(1);
  else
    print(0);
}

void main() {
    bool x; 
    bool y;
    x = true;
    y = false;
    printb(x || y);
    printb(x && y);
    x = false;
    y = true;
    printb(x || y);
    printb(x && y);
    y = false;
    printb(x || y);
    printb(x && y);
    x = true;
    y = true;
    printb(x || y);
    printb(x && y);

    y = false;
    if (x && y)
      printb(x);
    else
      printb(y);

    while(x || y) {
      printb(x);
      x = false;
    }
} 