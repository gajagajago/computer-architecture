import Types::*;
import Vector::*;

/* =============== MODIFY HERE FOR SIMULATION =============== */
// When you are using direct mapped cache, LinesPerSet must be 1.
typedef 1 WordsPerBlock;  // You can change this to 1, 2, 4,..
typedef 1 LinesPerSet;   // You can change this to 1, 2, 4,..
/* ========================================================== */


/* YOU CAN MODIFY TotalCacheSize IF U WANT TO SEE SET ASSOCIATIVITY EFFECT MORE! */
typedef 256 TotalCacheSize;  
typedef Vector#(WordsPerBlock, Data) Line;
typedef Line MemResp; // memresponse returns line(block)

typedef Bit#(TLog#(WordsPerBlock)) BlockOffset; // 0 
typedef Bit#(TLog#(LinesPerSet)) SetOffset; // 0

typedef TDiv#(TDiv#(TotalCacheSize, LinesPerSet), WordsPerBlock) NumOfSets; 
typedef Bit#(TLog#(NumOfSets)) CacheIndex;  // 8bits
typedef Bit#(TSub#(TSub#(TSub#(AddrSz, TLog#(NumOfSets)),SizeOf#(BlockOffset)),2)) CacheTag;
// -2 because currently block size = 1 word = 2^2 byte

