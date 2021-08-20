object function_and_data{

    //rational number problem

    class Rational( x:Int , y:Int = 1 ) {

    require(y>0 , "denominator should always be positive")

    private def gcd( a:Int , b:Int ):Int = {
        if ( b == 0 ) a
        else if ( b > a ) gcd( b , a )
        else gcd( b, a%b )
    }


    private var g = gcd( Math.abs(x) , y )

    def numer = x/g

    def denom = y/g


    def neg() : Rational = new Rational( -this.numer , this.denom )

    def -( that : Rational ) : Rational = new Rational( this.numer * that.denom - that.numer * this.denom  , this.denom * that.denom )

    @Override
    override def toString() : String = numer + "/" + denom
}

println("Rational number problem");

val a = new Rational(2,3);
printtln(a.neg.toString());

val x = new Rational(3,4);
val y = new Rational(5,8);
val z = new Rational(2,7);
val w = x-y-z;

println(w.toString());


//Account problem



class Account (y: Int , z : Double){

    val accountNumber : Int = y
    var balance : Double = z

    def withdraw( amount : Double ):Unit = this.balance -= amount

    def deposit ( amount : Double ):Unit = this.balance += amount

    def transfer( amount : Double , that : Account ): Unit = {
        this.balance =  this.balance - amount
        that.balance = that.balance + amount
    }


    @Override
    override def toString() : String = "Account Number : " + this.accountNumber + "\n Balance : " + this.balance

}

//money transfer
val acc1 = new Account(100, 40000);
val acc2 = new Account(101, 50);
accc1.transfer(50, acc2);
println(acc1).toString;

var bank : List[Account] = List(new Account(1,5000),new Account(2,11000),new Account(3,5400),new Account(4,-10),new Account(5,-50));

//list of account 
prinln("bank account list : ");
println(bank)

val negAcc = (a: List[account]) => a.map( b => b.filter(x => x.balance < 0));

//list of account with negative balance
prinln("bank account with negative balance :");
println(negAcc(bank));

val sumOfAccontBalance = (a:List[Account]) => a.reduce((b,c) => a.balance + c.balance);

//sum of account balance 

prinln("sum of bank accounts : ");
prinln(sumOfAccountBalance(bank));

val interest = (a:List[Account]) => a.map(x => {
    x.balance match{
        case y if y >= 0 => x.deposit(y*0.05);
        case _ => x.withdraw((Math.abs(y))*0.1);
    }
})



}