import java.util.ArrayList;
import java.util.HashMap;

// Block Chain should maintain only limited block nodes to satisfy the functions
// You should not have all the blocks added to the block chain in memory 
// as it would cause a memory overflow.

public class BlockChain {
    public static final int CUT_OFF_AGE = 10;
    private BlockNode maxHeightBlock; 
    public ArrayList<BlockNode> heads;
    public int height;
    public TransactionPool txPool;
    UTXOPool uPool = new UTXOPool();
    private HashMap<ByteArrayWrapper, BlockNode> H;

    
    // all information required in handling a block in block chain
    private class BlockNode {
       public Block b;
       public BlockNode parent;
       public ArrayList<BlockNode> children;
       public int height;
       // utxo pool for making a new block on top of this block
       private UTXOPool uPool;

       public BlockNode(Block b, BlockNode parent, UTXOPool uPool) {
          this.b = b;
          this.parent = parent;
          children = new ArrayList<BlockNode>();
          this.uPool = uPool;
          if (parent != null) {
             height = parent.height + 1;
             parent.children.add(this);
          } else {
             height = 1;
          }
       }

       public UTXOPool getUTXOPoolCopy() {
          return new UTXOPool(uPool);
       }
    }
    
    /**
     * create an empty block chain with just a genesis block. Assume {@code genesisBlock} is a valid
     * block
     */
    public BlockChain(Block genesisBlock) {
    	 UTXOPool uPool = new UTXOPool();
         Transaction coinbase = genesisBlock.getCoinbase();
         UTXO utxoCoinbase = new UTXO(coinbase.getHash(), 0);
         uPool.addUTXO(utxoCoinbase, coinbase.getOutput(0));
         BlockNode genesis = new BlockNode(genesisBlock, null, uPool);
         heads = new ArrayList<BlockNode>();
         heads.add(genesis);
         H = new HashMap<ByteArrayWrapper, BlockNode>();
         H.put(new ByteArrayWrapper(genesisBlock.getHash()), genesis);
         height = 1;
         maxHeightBlock = genesis;
         txPool = new TransactionPool();

         
    	
    }

    /** Get the maximum height block */
    public Block getMaxHeightBlock() {
    	return maxHeightBlock.b;
    }

    /** Get the UTXOPool for mining a new block on top of max height block */
    public UTXOPool getMaxHeightUTXOPool() {    
    	return maxHeightBlock.getUTXOPoolCopy();
    }

    /** Get the transaction pool to mine a new block */
    public TransactionPool getTransactionPool() {

       return txPool;
    }
    private BlockNode getBlock(byte[] blockHash) {
        ByteArrayWrapper hash = new ByteArrayWrapper(blockHash);
        return H.get(hash);
     }
    
    /* Check if the transactions of the block form a valid set of transactions 
     * corresponding to the utxo pool (similar validity as in Assignment 1)
     * If its a valid set, return the updated utxo pool 
     * and add the block's coinbase transaction to it.
     * If not a valid set, return null
     */
    private UTXOPool processBlockTxs(UTXOPool uPool, Block b) {
        ArrayList<Transaction> aTxs = b.getTransactions();
        Transaction[] txs = aTxs.toArray(new Transaction[0]);

        TxHandler handler = new TxHandler(uPool);
        Transaction[] rTxs = handler.handleTxs(txs);
        if (rTxs.length != txs.length)
           return null;
        
        uPool = handler.getUTXOPool();
        Transaction coinbase = b.getCoinbase();
        UTXO utxoCoinbase = new UTXO(coinbase.getHash(), 0);
        uPool.addUTXO(utxoCoinbase, coinbase.getOutput(0));
        return uPool;
     }

    /* update the transaction pool, removing transactions used by block and return it
     */
    private void updateTransactionPool(Block b) {
       ArrayList<Transaction> aTxs = b.getTransactions();
       for (Transaction tx : aTxs) {
          txPool.removeTransaction(tx.getHash());
       }
    }

    /**
     * Add {@code block} to the block chain if it is valid. For validity, all transactions should be
     * valid and block should be at {@code height > (maxHeight - CUT_OFF_AGE)}.
     * 
     * <p>
     * For example, you can try creating a new block over the genesis block (block height 2) if the
     * block chain height is {@code <=
     * CUT_OFF_AGE + 1}. As soon as {@code height > CUT_OFF_AGE + 1}, you cannot create a new block
     * at height 2.
     * 
     * @return true if block is successfully added
     */
    public boolean addBlock(Block block) {
        byte[] prevBlock = block.getPrevBlockHash();
    	if (prevBlock == null) return false;
    	BlockNode parent = getBlock(prevBlock);
    	if (parent == null) return false;
    	
        UTXOPool uPool = parent.getUTXOPoolCopy();
        uPool = processBlockTxs(uPool, block);
    	if (uPool == null) return false;
        updateTransactionPool(block); //update the transaction pool, removing transactions used by block 
        
        BlockNode current = new BlockNode(block, parent, uPool);
        H.put(new ByteArrayWrapper(block.getHash()), current);

        if (current.height > height) {
            maxHeightBlock = current;
            height = current.height;
         }
        
        
        if (height - heads.get(0).height > CUT_OFF_AGE) {
            ArrayList<BlockNode> newHeads = new ArrayList<BlockNode>();
            for (BlockNode bHeads : heads) {
               for (BlockNode bChild : bHeads.children) {
                  newHeads.add(bChild);
               }
               H.remove(new ByteArrayWrapper(bHeads.b.getHash()));
            }
            heads = newHeads;
         }
         return true;

    }

    /** Add a transaction to the transaction pool */
    public void addTransaction(Transaction tx) {
        txPool.addTransaction(tx);
    }
}