import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
/*double p_graph = .1; //Double.parseDouble(args[0]); // parameter for random graph: prob. that an edge will exist
double p_malicious = .15; //Double.parseDouble(args[1]); // prob. that a node will be set to be malicious
double p_txDistribution = .01; //Double.parseDouble(args[2]); // probability of assigning an initial transaction to each node 
int numRounds = 20; //Integer.parseInt(args[3]); // number of simulation rounds your nodes will run for
*/
/* CompliantNode refers to a node that follows the rules (not malicious)*/
public class CompliantNode implements Node {
	Set<Transaction> pendingTransactions ;
	boolean[] followees;
	double p_graph, p_malicious, p_txDistribution, numRounds;	
	
    public CompliantNode(double p_graph, double p_malicious, double p_txDistribution, int numRounds) {
    	this.p_graph=p_graph; this.p_malicious=p_malicious; this.p_txDistribution=p_txDistribution; this.numRounds=numRounds;
    }

    public void setFollowees(boolean[] followees) {
        this.followees = followees;
    }

    public void setPendingTransaction(Set<Transaction> pendingTransactions) {
        this.pendingTransactions = pendingTransactions;
    }

    public Set<Transaction> sendToFollowers() {   	//this.pendingTransactions = new HashSet<Transaction>();    	
        return this.pendingTransactions;    	
    }

    public void receiveFromFollowees(Set<Candidate> candidates) {
        HashMap<Integer, Integer> popular = new HashMap<>();

       for (Candidate c : candidates){
    	   if (followees[c.sender]){
    		   if (!popular.containsKey(c.tx.hashCode())){
    			   popular.put(c.tx.hashCode(),0);
    		   }
    		   int incr = popular.get(c.tx.hashCode());
    		   popular.put(c.tx.hashCode(), ++incr);
    		   if (//incr > 60 &
    				   !pendingTransactions.contains(c.tx) ){
    		   
    			   pendingTransactions.add(c.tx);
    	   }
       }
       }
    }
}
