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

//int_math_int64_pow_stub
//function int_math_int64_pow_stub(base, exponent){}

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
//Requires: caml_int64_mul, caml_int64_of_int32
var ms_to_nano = caml_int64_of_int32(1000*1000);
function core_kernel_time_ns_gettime_or_zero(){
    var ms = (new Date()).getTime();
    var ms_i64 = caml_int64_of_int32(ms);
    return caml_int64_mul(ms_i64,ms_to_nano);
}
//Provides: core_kernel_time_ns_strftime
function core_kernel_time_ns_strftime(tm,format){
    var d = new Date(tm[6]+1900,tm[5],tm[4],tm[3],tm[2],tm[1]);
    
}
/*
  core_kernel_gc_compactions
  core_kernel_gc_heap_chunks
  core_kernel_gc_heap_words
  core_kernel_gc_major_collections
  core_kernel_gc_major_plus_minor_words
  core_kernel_gc_major_words
  core_kernel_gc_minor_collections
  core_kernel_gc_minor_words
  core_kernel_gc_promoted_words
  core_kernel_gc_top_heap_words
*/	


///////// BIN_PROT
//Provides: bin_prot_get_float_offset
//Requires: caml_float_of_bytes, caml_ba_get_1
function bin_prot_get_float_offset(a,p){
    var t = new Array(8);;
    for (var i = 0;i < 8;i++) t[i] = caml_ba_get_1(a,p++);
    var v = caml_float_of_bytes (t);
    return [254,v];
}
