//-----------------------------------------------------------------------------
// Project       : fpga-hash-table
//-----------------------------------------------------------------------------
// Author        : Ivan Shevchuk (github/johan92)
//-----------------------------------------------------------------------------

package linked_list;
  
  parameter KEY_WIDTH        = 32;
  parameter TABLE_ADDR_WIDTH = 10;
  parameter HEAD_PTR_WIDTH   = TABLE_ADDR_WIDTH;

  typedef enum logic [1:0] {
    OP_INSERT,
    OP_DELETE,
    OP_DEQUEUE
  } ht_opcode_t;

  typedef enum logic [2:0] {
    INSERT_SUCCESS,
    INSERT_SUCCESS_SAME_KEY, 
    INSERT_NOT_SUCCESS_TABLE_IS_FULL,

    DELETE_SUCCESS,
    DELETE_NOT_SUCCESS_NO_ENTRY, 

    DEQUEUE_SUCCESS,
    DEQUEUE_NOT_SUCCESS_NO_ENTRY
  } ht_rescode_t;
  
  typedef enum logic [1:0] {
    READ_NO_HEAD,
    KEY_MATCH,
    KEY_NO_MATCH_HAVE_NEXT_PTR,
    GOT_TAIL
  } ht_data_table_state_t;
  
  typedef enum logic [2:0] {
    NO_CHAIN,

    IN_HEAD,
    IN_MIDDLE,
    IN_TAIL,

    IN_TAIL_NO_MATCH
  } ht_chain_state_t;

  typedef struct packed {
    logic [HEAD_PTR_WIDTH-1:0] ptr;
    logic                      ptr_val;
  } head_ram_data_t;

  typedef struct packed {
    logic [KEY_WIDTH-1:0]      key;
    logic [HEAD_PTR_WIDTH-1:0] next_ptr;
    logic                      next_ptr_val;
  } ram_data_t; 
  
  typedef struct packed {
    logic        [KEY_WIDTH-1:0]    key;
    ht_opcode_t                     opcode;
  } ht_command_t;
  
  // pdata - data to pipeline/proccessing
  typedef struct packed {
    ht_command_t                cmd;
    logic  [HEAD_PTR_WIDTH-1:0] head_ptr;
    logic                       head_ptr_val;
  } ht_pdata_t;

  typedef struct packed {
    ht_command_t                cmd;
    ht_rescode_t                rescode;
    
    // only for verification
    ht_chain_state_t            chain_state;
  } ht_result_t;

endpackage
