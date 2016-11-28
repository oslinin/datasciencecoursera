//package bchw1;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Signature;

public class main {

	public static void main(String[] args) {
		/*System.out.println("hello world");
		Transaction tr = new Transaction();
		System.out.println(tr.numInputs());
		UTXOPool utxop = new UTXOPool();
		System.out.println(utxop.toString());
	
        
        Signature sig2 = Signature.getInstance("SHA256withRSA");
        PublicKey.
///		Transaction.Output o1 = new Transaction.Output(10.0,(PublicKey)sig);
	//	Transaction.Output o2 = new Transaction.Output(20.0,(PublicKey)sig2);
        //Transaction x = new Transaction();
        //x.addInput(, outputIndex);
         * 	*/
		try {
			PublicKey sig = (PublicKey)Signature.getInstance("SHA256withRSA");
		} catch (NoSuchAlgorithmException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}
}
