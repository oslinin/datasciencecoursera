import java.security.PublicKey;
import java.security.Signature;
import java.util.ArrayList;a
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
 

public class TxHandler {
	
	
    public TxHandler(UTXOPool utxoPool) {
        utxoPoolL = new UTXOPool(utxoPool);
      }

    public boolean isValidTx(Transaction tx) {
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
    			if (!p) return false;
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
       	
    	return true;    
    	}
    public Transaction[] handleTxs(Transaction[] possibleTxs) {
    	ArrayList<Transaction> trs = new ArrayList<Transaction>(Arrays.asList(possibleTxs));
    	ArrayList<Transaction> trs2 = new ArrayList<Transaction>();
		

    	//trs.forEach((trx) -> {if (!isValidTx(trx)) trs.remove(trx);});
    	//for (Transaction tx1 : trs) if (!isValidTx(tx1)) trs.remove(tx1);
    	Transaction txr2 = new Transaction();
    	for (Transaction tx1 : trs){ 
    		if (isValidTx(tx1)) { // remove invalid transactions.
    			
    			//try adding tx1 to txr2; call it txr
    			Transaction txr = txr2;
    			for (Transaction.Input  ti : tx1.getInputs())  {
    				txr.addInput( ti.prevTxHash, ti.outputIndex);
    				txr.addSignature(ti.signature, txr.numInputs()-1);
    			}
    			for (Transaction.Output ti : tx1.getOutputs()){
    				txr.addOutput(ti.value, ti.address);
    			}
    			txr.finalize();
    			
    			if (isValidTx2(txr)) {
    				txr2=txr;
    				trs2.add(tx1);
    			}  
    		} //else {trs.remove(tx1);} // remove invalid transactions and those that mess up txr2 running transaction array.
    	}
    	Transaction[] x = new Transaction[trs2.size()];
    	for(int i =0; i<trs2.size(); i++) {
    		Transaction y = trs2.get(i);
    		x[i] = y;
    		for (int j =0; j<y.numInputs(); j++){
        		Transaction.Input ti = y.getInput(j);
    			UTXO txo = new UTXO(ti.prevTxHash, ti.outputIndex);
    			utxoPoolL.removeUTXO(txo);
    		}
    	}
    	return  x;
    }
	UTXOPool utxoPoolL;

}
