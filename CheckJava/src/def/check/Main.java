package def.check;

import java.util.Arrays;

public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int[] coins = {3,2,1};
		int total = 10;
		System.out.println("so many combinations possible " + findCombo(total, coins));
	}
	
	static int findCombo(int amount, int[] coins){
		System.out.println("Calling findCombo amount " + amount + " coins " + Arrays.toString(coins));
		if(coins.length == 1) {
			System.out.println("This is the evaluation for findCombo amount " + amount + " coins " + coins[0] + " " + amount % coins[0]);
			return amount % coins[0] == 0 ? 1 : 0;
		}
		else {
			int total = 0;
			int[] subcoins = arrayofCoinsExceptTheFirstOne(coins);
			for(int i=0; i*coins[0] <= amount; ++i){
				System.out.println("i is : " + i);
				total += findCombo(amount-i*coins[0], subcoins);
				System.out.println(total);
			}
			return total;
			
		}
	}

	static int[] arrayofCoinsExceptTheFirstOne(int[] coins) {
		int[] subcoins = new int[coins.length-1];
		for(int i=1; i< coins.length; i++)
			subcoins[i-1] = coins[i];
		return subcoins;
	}
}
