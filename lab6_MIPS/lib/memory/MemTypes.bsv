typedef 32 AddrSz;
typedef 256 Rows;
typedef Bit#(AddrSz) Addr;
typedef Bit#(TLog#(Rows)) Index;
typedef Bit#(TSub#(AddrSz, TAdd#(TLog#(Rows), 2))) Tag;

typedef 32 DataSz;
typedef Bit#(DataSz) Data;

typedef enum{Ld, St} MemOp deriving(Eq,Bits);
typedef struct{
    MemOp op;
    Addr  addr;
    Data  data;
} MemReq deriving(Eq,Bits);

typedef Data MemResp;

