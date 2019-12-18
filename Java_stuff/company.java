import java.util.Scanner;
public class company {
    String item1 ;
    int item1Price ;
    String item2 ;
    int item2Price;
    String item3;
    int item3Price;
    int itemAmount1 = 20;
    int itemAmount2 = 20;
    int itemAmount3 = 20;
    int profit = 0;
    public company(String a, String b, String c, int a1, int a2, int a3){
        this.item1 = a;
        this.item1Price = a1;
        this.item2 = b;
        this.item2Price = a2;
        this.item3 = c;
        this.item3Price = a3;
    }

    public boolean increaseStock(String stock, int amount){
        Scanner inputScanner = new Scanner(System.in);
        if( stock.equalsIgnoreCase(this.item1)){
            System.out.print("How many of this  stock do you want to sell: ");
            if(inputScanner.hasNextInt()){
                amount = inputScanner.nextInt();
                if( amount <= itemAmount1){
                    this.itemAmount1 = this.itemAmount1 - amount;

                }
            }
        }
        else if( stock.equalsIgnoreCase(this.item2)){

        }
        else if( stock.equalsIgnoreCase(this.item3)){

        }
        else{
            System.out.println("This store does not sell that  stock")
        }
    }
}
