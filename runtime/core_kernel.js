///////// BIGSTRING
//Provides: bigstring_alloc
//Requires: caml_ba_create
function bigstring_alloc(_,size){
    return caml_ba_create(12, 0, [0,size]);
}

///////// CORE_KERNEL
//Provides: int_math_int_pow_stub
function int_math_int_pow_stub(base, exponent){
    return Math.pow(base | 0, exponent | 0) | 0;
}

//Provides: int_math_int64_pow_stub
//Requires: caml_int64_mul, caml_int64_is_zero, caml_int64_shift_right_unsigned
function int_math_int64_pow_stub(base, exponent){
    var one = [255,1,0,0];
    var mul = [one, base, one, one];
    var res = one;
    while (!caml_int64_is_zero(exponent)) {
      mul[1] = caml_int64_mul(mul[1],mul[3]);
      mul[2] = caml_int64_mul(mul[1],mul[1]);
      mul[3] = caml_int64_mul(mul[2],mul[1]);
      res = caml_int64_mul(res, mul[exponent[1]& 3]);
      exponent = caml_int64_shift_right_unsigned(exponent, 2);
    }
    return res;
}

//Provides: caml_make_float_vect
function caml_make_float_vect(len){
  var len = len + 1 | 0;
  var b = new Array(len);
  b[0]=254;
  return b
}
//Provides: caml_hash_string
//Requires: caml_hash
function caml_hash_string(s) {
  return caml_hash(1,1,0,s)
}
//Provides: caml_hash_double
//Requires: caml_hash
function caml_hash_double(d) {
  return caml_hash(1,1,0,d);
}

//Provides: core_heap_block_is_heap_block
function core_heap_block_is_heap_block(x){
    return +(x instanceof Array);
}

//Provides: core_array_unsafe_int_blit
//Requires: caml_array_blit
var core_array_unsafe_int_blit = caml_array_blit
//Provides: core_array_unsafe_float_blit
//Requires: caml_array_blit
var core_array_unsafe_float_blit = caml_array_blit

//Provides: core_kernel_time_ns_gettime_or_zero
//Requires: caml_int64_mul, caml_int64_of_float
var ms_to_nano = caml_int64_of_int32(1000*1000);
function core_kernel_time_ns_gettime_or_zero(){
    var ms = (new Date()).getTime();
    var ms_i64 = caml_int64_of_float(ms);
    return caml_int64_mul(ms_i64,ms_to_nano);
}
//Provides: core_kernel_time_ns_strftime
function core_kernel_time_ns_strftime(tm,format){
    var d = new Date(tm[6]+1900,tm[5],tm[4],tm[3],tm[2],tm[1]);
    
}

//Provides: core_kernel_gc_compactions
function core_kernel_gc_compactions () { return 0 }
//Provides: core_kernel_gc_heap_chunks
function core_kernel_gc_heap_chunks () { return 0 }
//Provides: core_kernel_gc_heap_words
function core_kernel_gc_heap_words () { return 0 }
//Provides: core_kernel_gc_major_collections
function core_kernel_gc_major_collections () { return 0 }
//Provides: core_kernel_gc_major_plus_minor_words
function core_kernel_gc_major_plus_minor_words () { return 0 }
//Provides: core_kernel_gc_major_words
function core_kernel_gc_major_words () { return 0 }
//Provides: core_kernel_gc_minor_collections
function core_kernel_gc_minor_collections () { return 0 }
//Provides: core_kernel_gc_minor_words
function core_kernel_gc_minor_words () { return 0 }
//Provides: core_kernel_gc_promoted_words
function core_kernel_gc_promoted_words () { return 0 }
//Provides: core_kernel_gc_top_heap_words
function core_kernel_gc_top_heap_words () { return 0 }
//Provides: caml_gc_counters
function caml_gc_counters() { return [254,0,0,0] }
//Provides: caml_gc_quick_stat
function caml_gc_quick_stat(){
    return [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
}
//Provides: caml_gc_stat
function caml_gc_stat() {
    return [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
}

//Provides: core_kernel_time_ns_nanosleep
//Requires: caml_failwith
function core_kernel_time_ns_nanosleep () {
  caml_failwith ("core_kernel_time_ns_nanosleep: impossible return value from nanosleep(2)")
}

//Provides: clear_caml_backtrace_pos
function clear_caml_backtrace_pos () { return 0 }

///////// BIN_PROT
//Provides: bin_prot_get_float_offset
//Requires: caml_float_of_bytes, caml_ba_get_1
function bin_prot_get_float_offset(a,p){
    var t = new Array(8);;
    for (var i = 0;i < 8;i++) t[i] = caml_ba_get_1(a,p++);
    var v = caml_float_of_bytes (t);
    return [254,v];
}


// bigstring_blit_bigstring_string_stub
// bigstring_blit_string_bigstring_stub
// bigstring_blit_stub
// bin_prot_blit_buf_float_array_stub
// bin_prot_blit_buf_string_stub
// bin_prot_blit_float_array_buf_stub
// bin_prot_blit_string_buf_stub
// caml_ba_uint8_get16
// caml_ba_uint8_get64
// caml_ba_uint8_set16
// caml_ba_uint8_set64
// caml_bswap16
// caml_int64_bswap
