import java.security.PublicKey;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
public class MaxFeeTxHandler {
    public MaxFeeTxHandler(UTXOPool utxoPool) {
    	utxoPoolL = new UTXOPool(utxoPool);
    }

    /**@return true if:
     * (1) all outputs claimed by code tx are in the current UTXO pool, 
     * (2) the signatures on each input of code tx are valid, 
     * (3) no UTXO is claimed multiple times by code tx,
     * (4) all of code txs output values are non-negative, and
     * (5) the sum of code txs input values is greater than or equal to the sum of its output
     *     values; and false otherwise.     */
    public boolean isValidTx(Transaction tx) {

        boolean ok1 = true;
        ArrayList<UTXO> inputslist = new ArrayList<UTXO>();
        double intot=0;
    	for (int i = 0 ; i < tx.numInputs() ; i++) { //iterate inputs
    		byte[] rawdata = tx.getRawDataToSign(i);
    		Transaction.Input ti = tx.getInput(i);
    		UTXO txo = new UTXO(ti.prevTxHash, ti.outputIndex);
    		ok1 = ok1 & 
    		        // (1) all outputs claimed by code tx are in the current UTXO pool,
    				utxoPoolL.contains(txo) &
    				// (2) the signatures on each input of code tx are valid,		
    				Crypto.verifySignature(utxoPoolL.getTxOutput(txo).address, rawdata , ti.signature);    		
    		intot += utxoPoolL.getTxOutput(txo).value;
    		inputslist.add(txo);
     	}
    	
    	Collections.sort(inputslist);
    	for (int i = 2; i < inputslist.size(); i++) // (3) no UTXO is claimed multiple times by code tx,
    		if (inputslist.get(i).equals(inputslist.get(i-1))) ok1=false;
    	
    	double outtot=0;
       	for (int i = 0 ; i < tx.numOutputs() ; i++) { //iterate outputs
       		if (tx.getOutput(i).value<0) ok1=false; // (4)  all of code txs output values are non-negative
       		outtot += tx.getOutput(i).value;
       	}
       	if (outtot>intot) ok1=false; //(5) the sum of code txs input values is greater than or equal to the sum of its output values; and false otherwise.
       	
    	return ok1;
    }
    public boolean isValidTx2(Transaction tx) {
    	//boolean ok1 = true;
        ArrayList<UTXO> inputslist = new ArrayList<UTXO>();
        double intot=0;
    	for (int i = 0 ; i < tx.numInputs() ; i++) { //iterate inputs
    		byte[] rawdata = tx.getRawDataToSign(i);
    		Transaction.Input ti = tx.getInput(i);
    		UTXO txo = new UTXO(ti.prevTxHash, ti.outputIndex);
    		inputslist.add(txo);
    		//ok1 = ok1 & 
    		        // (1) all outputs claimed by code tx are in the current UTXO pool,
    		if (!utxoPoolL.contains(txo)) return false; 
    				// (2) the signatures on each input of code tx are valid,
    		
    			//ok1=ok1&	Crypto.verifySignature(utxoPoolL.getTxOutput(txo).address, rawdata , ti.signature);
    		try{
    			Transaction.Output o = utxoPoolL.getTxOutput(txo);     	
    			RSAKey k = o.address;
    			byte[] sig = tx.getInput(i).signature;
    			boolean p = k.verifySignature(tx.getRawDataToSign(i) , sig); 
    		//	if (!p) return false;
    		} catch (Exception e) {
    			return false;
    		}
    		
    		
                try {
                	Transaction.Output x = utxoPoolL.getTxOutput(txo);
                    intot += x.value;
                } catch(NullPointerException e)    {    
                    return false;
                } catch(Exception e2){
                	return false;
                }
    		
     	}
    	
    	Collections.sort(inputslist);
    	for (int i = 1; i < inputslist.size(); i++) // (3) no UTXO is claimed multiple times by code tx,
    		if (inputslist.get(i).equals(inputslist.get(i-1))) return false;
    	
    	double outtot=0;
       	for (int i = 0 ; i < tx.numOutputs() ; i++) { //iterate outputs
       		if (tx.getOutput(i).value<0) return false; // (4)  all of code txs output values are non-negative
       		outtot += tx.getOutput(i).value;
       	}
       	if (outtot>intot) return false; //(5) the sum of code txs input values is greater than or equal to the sum of its output values; and false otherwise.
       	
    	return true;    }
     /**
     * Handles each epoch by receiving an unordered array of proposed transactions, checking each
     * transaction for correctness, returning a mutually valid array of accepted transactions, and
     * updating the current UTXO pool as appropriate.
     */
    public class Transaction2{
    	public Transaction t;
    	public ArrayList<Integer> i;    
    	public Transaction2(Transaction x, ArrayList<Integer> j){t=x; i=j;}
    	public Transaction2(Transaction x,int j){t=x; i=new ArrayList<Integer>(0); i.add(j);}
    	public void bindTx2(Transaction x, int j){t = bindTx(t, x); i.add(j);}    	  
    }
    public Transaction[] handleTxs(Transaction[] possibleTxs) {
    	ArrayList<Transaction> trs = new ArrayList<Transaction>(Arrays.asList(possibleTxs));
    	for (Transaction tx1 : trs){if (!isValidTx(tx1)) trs.remove(tx1);}
    	int tot = (int)Math.ceil(totalValue(trs)*1);
    	int[][] K = new int[trs.size()][tot];   	
    	Transaction2[][] T = new Transaction2[trs.size()][tot];
    	//Knapsack where weight = value.
    	int a=0,b=0,v=0,i=0,j=0;
    	v = (int)totalValue(trs.get(0));
    	for (j = 0; j<= tot;  j++) if (j>=v) {K[0][j]=v; T[0][j]=new Transaction2(trs.get(0), 0);}
    	
    	
    	for (i = 1; i< trs.size();  i++){ //transactions = x
    		v = (int)totalValue(trs.get(i));
    		for (j = 0; j<tot; j++){ //volumes = y   			
    			a = K[i-1][j];
    			//b = K[i-1][j-v] + v;
    			if (j<v){
    				K[i][j]=a; T[i][j]=T[i-1][j];
    			} else {
        			Transaction2 n =T[i-1][j-v];
        			n.bindTx2(trs.get(i),i);    			
        			if (!isValidTx2(n.t)) b=0; else b=totalValue(n.t);
        			if (b>a){ 
        				K[i][j]=b; T[i][j]=n;
        			} else {
    					K[i][j]=a; T[i][j]=T[i-1][j];
    				}
    			}
    		}
    	}
	int best=0;
	for (i=0; i<trs.size(); i++)
		for (j=0; j<tot; j++)
			if(K[i][j]>best) {
				best=K[i][j];a=i; b=j;
			}
        
    	Transaction[] res=new Transaction[T[a][b].i.size()];
    	for( int k = 0 ; k < res.length; k++) res[k]=trs.get(k);
    	return res;
    }


 
    public int totalValue(Transaction tx){
    	double outtot=0;     	
       		for (int j = 0; j<tx.numOutputs(); j++){
       			outtot += tx.getOutput(j).value;
       		}       	
       	return (int)outtot;
    }
    public double totalValue(ArrayList<Transaction> trs){
    	return totalValue((Transaction[])trs.toArray());  
    }
    public double totalValue(Transaction[] tx){
    	double outtot=0;
       	for (int i = 0 ; i < tx.length ; i++) { //iterate outputs
       		for (int j = 0; j<tx[i].numOutputs(); j++){
       			outtot += tx[i].getOutput(j).value;
       		}
       	}
       	return outtot;
    }
	public Transaction bindTx(Transaction txr2, Transaction tx1){
		if (txr2==null & tx1==null) return null;
		if (txr2==null) return tx1;
		if (tx1==null) return txr2;
		Transaction txr = new Transaction(txr2);
		for (Transaction.Input  ti : tx1.getInputs())  txr.addInput( ti.prevTxHash, ti.outputIndex);
		for (Transaction.Output ti : tx1.getOutputs()) txr.addOutput(ti.value, ti.address);
		txr.finalize();
		return txr;
	}
	
    UTXOPool utxoPoolL;
}
