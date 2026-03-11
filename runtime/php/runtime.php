<?php
// Ocamlephan Minimal Bridge
set_time_limit(10);

// Exception wrapper pour OCaml values
class CamlException extends Exception {
    public $caml_val;
    public function __construct($v) {
        parent::__construct("OCaml Exception");
        $this->caml_val = $v;
    }
}

// OCaml Block representation (to have reference semantics)
class CamlBlock implements ArrayAccess, Countable, IteratorAggregate {
    public array $fields;
    public function __construct(array $fields) {
        $this->fields = $fields;
    }
    public function offsetExists($offset): bool {
        return array_key_exists($offset, $this->fields);
    }
    #[\ReturnTypeWillChange]
    public function &offsetGet($offset) {
        if (!array_key_exists($offset, $this->fields)) {
            $null = null;
            return $null;
        }
        return $this->fields[$offset];
    }
    public function offsetSet($offset, $value): void {
        $this->fields[$offset] = $value;
    }
    public function offsetUnset($offset): void {
        unset($this->fields[$offset]);
    }
    public function count(): int {
        return count($this->fields);
    }
    public function getIterator(): Traversable {
        return new ArrayIterator($this->fields);
    }
}

// Gestion de l'arité pour PHP Closures
$GLOBALS['caml_arities'] = [];
function caml_set_arity($f, $arity) {
    if (is_object($f)) {
        $GLOBALS['caml_arities'][spl_object_hash($f)] = $arity;
    }
}
$GLOBALS['caml_set_arity'] = 'caml_set_arity';

function caml_get_arity($f) {
    if (!is_object($f)) {
        if (is_string($f) && is_callable($f)) {
            try {
                $ref = new ReflectionFunction($f);
                return $ref->getNumberOfParameters();
            } catch (Exception $e) { return -1; }
        }
        return -1;
    }
    if ($f instanceof CamlBlock) return -1;
    $hash = spl_object_hash($f);
    if (isset($GLOBALS['caml_arities'][$hash])) return $GLOBALS['caml_arities'][$hash];
    if ($f instanceof Closure || is_callable($f)) {
        try {
            $ref = new ReflectionFunction($f);
            return $ref->getNumberOfParameters();
        } catch (Exception $e) { return -1; }
    }
    return -1;
}
$GLOBALS['caml_get_arity'] = 'caml_get_arity';

// Helpers pour la traduction JSOO -> PHP
function caml_typeof($x) {
    if (is_int($x) || is_float($x)) return "number";
    if (is_string($x)) return "string";
    if (is_bool($x)) return "boolean";
    if (is_array($x) || is_object($x)) return "object";
    return "undefined";
}
$GLOBALS['caml_typeof'] = 'caml_typeof';

function caml_js_length($x) {
    if ($x instanceof CamlBlock) return count($x->fields);
    if (is_array($x)) return count($x);
    if (is_string($x)) return strlen($x);
    if (is_object($x) && isset($x->length)) return $x->length;
    return 0;
}
$GLOBALS['caml_js_length'] = 'caml_js_length';

function caml_check_bound($arr, $i) { return $arr; }
function caml_check_bound_gen($arr, $i) { return $arr; }
function caml_seq($e1, $e2) { return $e2; }

function caml_print_string($s) { echo $s; }
function caml_print_int($i) { echo $i; }
function caml_print_newline() { echo PHP_EOL; }

// Simulation des blocs JSOO en PHP
function caml_make_block($tag, $fields) {
    return new CamlBlock(array_merge([$tag], $fields));
}

if (!isset($GLOBALS['globalThis'])) $GLOBALS['globalThis'] = new stdClass();
$globalThis = $GLOBALS['globalThis'];

// Helper to set both $globalThis->prop and $GLOBALS['prop']
function _jsoo_set_global($name, $value) {
    $GLOBALS[$name] = $value;
    $GLOBALS['globalThis']->$name = $value;
}

_jsoo_set_global('caml_set_arity', 'caml_set_arity');
_jsoo_set_global('caml_get_arity', 'caml_get_arity');
_jsoo_set_global('caml_typeof', 'caml_typeof');
_jsoo_set_global('caml_js_length', 'caml_js_length');
_jsoo_set_global('caml_check_bound', 'caml_check_bound');
_jsoo_set_global('caml_check_bound_gen', 'caml_check_bound_gen');
_jsoo_set_global('caml_seq', 'caml_seq');

_jsoo_set_global('caml_maybe_attach_backtrace', function($exn, $i) { return $exn; });

_jsoo_set_global('caml_register_global', function($id, $v, $name) {
    $GLOBALS[$name] = $v;
});

_jsoo_set_global('caml_fs_init', function() { });

_jsoo_set_global('caml_ml_open_descriptor_in', function($fd) { return $fd; });
_jsoo_set_global('caml_ml_open_descriptor_out', function($fd) { return $fd; });

_jsoo_set_global('caml_ml_output', function($fd, $s, $start, $len) {
    echo substr($s, $start, $len);
});

_jsoo_set_global('caml_ml_output_char', function($fd, $c) {
    echo chr($c);
});

_jsoo_set_global('caml_ml_flush', function($fd) { });

_jsoo_set_global('caml_ml_out_channels_list', function() {
    return 0;
});

$GLOBALS['caml_oo_last_id'] = 0;
_jsoo_set_global('caml_fresh_oo_id', function() {
    return $GLOBALS['caml_oo_last_id']++;
});
_jsoo_set_global('caml_set_oo_id', function($b) {
    $b[2] = $GLOBALS['caml_oo_last_id']++;
    return $b;
});

_jsoo_set_global('caml_wrap_exception', function($e) {
    if ($e instanceof CamlException) return $e->caml_val;
    return new CamlBlock([0, 0, $e]);
});

_jsoo_set_global('caml_failwith', function($s) {
    throw new Exception($s);
});

_jsoo_set_global('caml_ml_string_length', function($s) {
    return strlen($s);
});

_jsoo_set_global('caml_int_compare', function($a, $b) {
    return $a <=> $b;
});

// Sys primitives (as functions)
_jsoo_set_global('caml_sys_io_buffer_size', function() { return 4096; });
_jsoo_set_global('caml_sys_get_config', function() { return new CamlBlock([0, "ocaml", 8, "unix"]); });
_jsoo_set_global('caml_sys_executable_name', function() { return "php"; });
_jsoo_set_global('caml_sys_const_ostype_unix', function() { return 1; });
_jsoo_set_global('caml_sys_const_ostype_win32', function() { return 0; });
_jsoo_set_global('caml_sys_const_ostype_cygwin', function() { return 0; });
_jsoo_set_global('caml_sys_const_max_wosize', function() { return 0x7FFFFFFF; });

// Bytes primitives
_jsoo_set_global('caml_create_bytes', function($len) { return str_repeat("\0", $len); });
_jsoo_set_global('caml_bytes_set', function(&$b, $i, $c) {
    $b[$i] = chr($c);
});
_jsoo_set_global('caml_bytes_of_string', function($s) { return $s; });
_jsoo_set_global('caml_ml_bytes_length', function($b) { return strlen($b); });
_jsoo_set_global('caml_blit_bytes', function($s, $si, &$d, $di, $len) {
    for ($i = 0; $i < $len; $i++) $d[$di + $i] = $s[$si + $i];
});

// Objects & Lazy primitives
_jsoo_set_global('caml_obj_tag', function($x) {
    if ($x instanceof CamlBlock) return $x->fields[0];
    if (is_array($x)) return $x[0];
    return 1000; // Tag for non-blocks
});
_jsoo_set_global('caml_obj_block', function($tag, $len) {
    return new CamlBlock(array_merge([$tag], array_fill(0, $len, 0)));
});
_jsoo_set_global('caml_atomic_fetch_add_field', function($obj, $i, $v) {
    $old = $obj[$i];
    $obj[$i] += $v;
    return $old;
});

$GLOBALS['caml_method_cache'] = [];
_jsoo_set_global('caml_oo_cache_id', function() {
    $id = count($GLOBALS['caml_method_cache']);
    $GLOBALS['caml_method_cache'][] = 0;
    return $id;
});

_jsoo_set_global('caml_get_public_method', function($obj, $tag) {
    $meths = $obj[1];
    $li = 3;
    $hi = $meths[1] * 2 + 1;
    while ($li < $hi) {
        $mi = (($li + $hi) >> 1) | 1;
        if ($tag < $meths[$mi + 1]) $hi = $mi - 2;
        else $li = $mi;
    }
    return ($tag === $meths[$li + 1]) ? $meths[$li] : 0;
});

_jsoo_set_global('caml_get_cached_method', function($obj, $tag, $cache_id) {
    $meths = $obj[1];
    $ofs = $GLOBALS['caml_method_cache'][$cache_id];
    if (isset($meths[$ofs + 4]) && $meths[$ofs + 4] === $tag) {
        return $meths[$ofs + 3];
    }
    $li = 3;
    $hi = $meths[1] * 2 + 1;
    while ($li < $hi) {
        $mi = (($li + $hi) >> 1) | 1;
        if ($tag < $meths[$mi + 1]) $hi = $mi - 2;
        else $li = $mi;
    }
    $GLOBALS['caml_method_cache'][$cache_id] = $li - 3;
    return $meths[$li];
});

_jsoo_set_global('caml_lazy_reset_to_lazy', function($lazy, $f) {
    $lazy[0] = 246; // Forcing
    $lazy[1] = $f;
});
_jsoo_set_global('caml_lazy_update_to_forcing', function($lazy, $f) {
    $lazy[0] = 246;
    $lazy[1] = $f;
});
_jsoo_set_global('caml_lazy_update_to_forward', function($lazy, $v) {
    $lazy[0] = 250; // Forward
    $lazy[1] = $v;
});
_jsoo_set_global('caml_alloc_dummy_lazy', function() { return new CamlBlock([246, 0]); });
_jsoo_set_global('caml_update_dummy_lazy', function($lazy, $v) {
    $lazy[0] = $v[0];
    $lazy[1] = $v[1];
});

// Array primitives
_jsoo_set_global('caml_array_make', function($len, $v) {
    return new CamlBlock(array_merge([0], array_fill(0, $len, $v)));
});
_jsoo_set_global('caml_array_blit', function($s, $si, $d, $di, $len) {
    for ($i = 0; $i < $len; $i++) $d[$di + $i + 1] = $s[$si + $i + 1];
});

// String primitives
_jsoo_set_global('caml_string_compare', function($a, $b) {
    return strcmp($a, $b);
});
_jsoo_set_global('caml_string_get', function($s, $i) {
    return ord($s[$i]);
});

// Bitwise shift right unsigned (JS >>> operator)
function caml_shift_right_unsigned($a, $b) {
    $a = (int)$a;
    $b = (int)$b;
    if ($b >= 32) return 0;
    return ($a & 0xFFFFFFFF) >> $b;
}
_jsoo_set_global('caml_shift_right_unsigned', 'caml_shift_right_unsigned');

_jsoo_set_global('caml_call_gen', function($f, $args) {
    if ($args instanceof CamlBlock) $args = $args->fields;
    $n = count($args);
    if ($n === 0) return $f;
    $arity = caml_get_arity($f);
    if ($arity <= 0) {
        return $f(...$args);
    }
    if ($arity === $n) {
        return $f(...$args);
    }
    if ($arity < $n) {
        $res = $f(...array_slice($args, 0, $arity));
        return $GLOBALS['caml_call_gen']($res, array_slice($args, $arity));
    }
    // Application partielle
    return function(...$extra_args) use ($f, $args) {
        return $GLOBALS['caml_call_gen']($f, array_merge($args, $extra_args));
    };
});

_jsoo_set_global('caml_call1', function($f, $a0) {
    return $GLOBALS['caml_call_gen']($f, [$a0]);
});
_jsoo_set_global('caml_call2', function($f, $a0, $a1) {
    return $GLOBALS['caml_call_gen']($f, [$a0, $a1]);
});
_jsoo_set_global('caml_call3', function($f, $a0, $a1, $a2) {
    return $GLOBALS['caml_call_gen']($f, [$a0, $a1, $a2]);
});
_jsoo_set_global('caml_call4', function($f, $a0, $a1, $a2, $a3) {
    return $GLOBALS['caml_call_gen']($f, [$a0, $a1, $a2, $a3]);
});
_jsoo_set_global('caml_call5', function($f, $a0, $a1, $a2, $a3, $a4) {
    return $GLOBALS['caml_call_gen']($f, [$a0, $a1, $a2, $a3, $a4]);
});
