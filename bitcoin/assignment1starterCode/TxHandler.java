import java.security.PublicKey;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;

public class TxHandler {
    /**Creates a public ledger whose current UTXOPool (collection of unspent transaction outputs) is
     * {@code utxoPool}. This should make a copy of utxoPool by using the UTXOPool(UTXOPool uPool)
     * constructor.     */
    public TxHandler(UTXOPool utxoPool) {
    	utxoPoolL = new UTXOPool(utxoPool);
    }

    /**@return true if:
     * (1) all outputs claimed by {@code tx} are in the current UTXO pool, 
     * (2) the signatures on each input of {@code tx} are valid, 
     * (3) no UTXO is claimed multiple times by {@code tx},
     * (4) all of {@code tx}s output values are non-negative, and
     * (5) the sum of {@code tx}s input values is greater than or equal to the sum of its output
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
    		        // (1) all outputs claimed by {@code tx} are in the current UTXO pool,
    				utxoPoolL.contains(txo) &
    				// (2) the signatures on each input of {@code tx} are valid,		
    				Crypto.verifySignature(utxoPoolL.getTxOutput(txo).address, rawdata , ti.signature);    		
    		intot += utxoPoolL.getTxOutput(txo).value;
    		inputslist.add(txo);
     	}
    	
    	Collections.sort(inputslist);
    	for (int i = 2; i < inputslist.size(); i++) // (3) no UTXO is claimed multiple times by {@code tx},
    		if (inputslist.get(i).equals(inputslist.get(i-1))) ok1=false;
    	
    	double outtot=0;
       	for (int i = 0 ; i < tx.numOutputs() ; i++) { //iterate outputs
       		if (tx.getOutput(i).value<0) ok1=false; // (4)  all of {@code tx}s output values are non-negative
       		outtot += tx.getOutput(i).value;
       	}
       	if (outtot>intot) ok1=false; //(5) the sum of {@code tx}s input values is greater than or equal to the sum of its output values; and false otherwise.
       	
    	return ok1;
    }

    /**
     * Handles each epoch by receiving an unordered array of proposed transactions, checking each
     * transaction for correctness, returning a mutually valid array of accepted transactions, and
     * updating the current UTXO pool as appropriate.
     */
    public Transaction[] handleTxs(Transaction[] possibleTxs) {
    	ArrayList<Transaction> trs = new ArrayList<Transaction>(Arrays.asList(possibleTxs));
    	
    	//trs.forEach((trx) -> {if (!isValidTx(trx)) trs.remove(trx);});
    	//for (Transaction tx1 : trs) if (!isValidTx(tx1)) trs.remove(tx1);
    	Transaction txr2 = new Transaction();
    	for (Transaction tx1 : trs){ 
    		if (isValidTx(tx1)) { // remove invalid transactions.
    			
    			//try adding tx1 to txr2; call it txr
    			Transaction txr = new Transaction(txr2);
    			for (Transaction.Input  ti : tx1.getInputs())  txr.addInput( ti.prevTxHash, ti.outputIndex);
    			for (Transaction.Output ti : tx1.getOutputs()) txr.addOutput(ti.value, ti.address);
    			txr.finalize();
    			
    			if (isValidTx(txr)) {
    				txr2=txr;
    				break;
    			} 
    		} // end if (isValidTx(tx1)) {
    		trs.remove(tx1); // remove invalid transactions and those that mess up txr2 running transaction array.
    	}
        return  (Transaction[])trs.toArray();
    }
    UTXOPool utxoPoolL;
}
