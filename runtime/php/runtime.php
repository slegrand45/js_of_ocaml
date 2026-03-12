<?php
// Ocamlephan Minimal Bridge
set_time_limit(10);
error_reporting(E_ALL & ~E_WARNING & ~E_NOTICE & ~E_DEPRECATED);

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
    if ($s instanceof CamlBlock) $s = $s->fields[1];
    if (is_object($s) && !($s instanceof CamlBlock)) {
        fwrite(STDERR, "DEBUG: caml_ml_output called with object of type " . get_class($s) . "\n");
        if ($s instanceof Closure) {
            fwrite(STDERR, "DEBUG: Closure is a closure!\n");
        }
    }
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
    if ($e instanceof Throwable) return new CamlBlock([0, 0, $e->getMessage()]);
    return $e;
});

_jsoo_set_global('caml_failwith', function($s) {
    throw new Exception($s);
});

_jsoo_set_global('caml_ml_string_length', function($s) {
    return is_string($s) ? strlen($s) : 0;
});

_jsoo_set_global('caml_int_compare', function($a, $b) {
    return $a <=> $b;
});

_jsoo_set_global('caml_format_float', function($fmt, $x) {
    return sprintf($fmt, $x);
});

_jsoo_set_global('caml_bytes_get', function($s, $i) {
    return ord($s[$i]);
});

_jsoo_set_global('caml_string_get', function($s, $i) {
    return ord($s[$i]);
});

// Sys primitives
_jsoo_set_global('caml_sys_io_buffer_size', function() { return 4096; });
_jsoo_set_global('caml_sys_get_config', function() { return new CamlBlock([0, "ocaml", 8, "unix"]); });
_jsoo_set_global('caml_sys_executable_name', function() { return "php"; });
_jsoo_set_global('caml_sys_const_ostype_unix', function() { return 1; });
_jsoo_set_global('caml_sys_const_ostype_win32', function() { return 0; });
_jsoo_set_global('caml_sys_const_ostype_cygwin', function() { return 0; });
_jsoo_set_global('caml_sys_const_max_wosize', function() { return 0x7FFFFFFF; });

// Bytes primitives
_jsoo_set_global('caml_create_bytes', function($len) {
    $len = max(0, (int)$len);
    return str_repeat("\0", $len);
});
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
    if (is_array($x)) return isset($x[0]) ? $x[0] : 1000;
    return 1000;
});
_jsoo_set_global('caml_obj_block', function($tag, $len) {
    return new CamlBlock(array_merge([$tag], array_fill(0, $len, 0)));
});
_jsoo_set_global('caml_obj_dup', function($x) {
    if ($x instanceof CamlBlock) return new CamlBlock($x->fields);
    return $x;
});

_jsoo_set_global('caml_atomic_fetch_add_field', function($obj, $i, $v) {
    $old = $obj[$i];
    $obj[$i] += $v;
    return $old;
});
_jsoo_set_global('caml_atomic_cas_field', function($obj, $i, $old, $v) {
    if ($obj[$i] === $old) {
        $obj[$i] = $v;
        return 1;
    }
    return 0;
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

_jsoo_set_global('caml_trampoline', function($res) {
    while ($res instanceof stdClass && isset($res->joo_tramp)) {
        $res = ($res->joo_tramp)(...$res->joo_args);
    }
    return $res;
});

_jsoo_set_global('caml_trampoline_return', function($f, $args, $direct) {
    $res = new stdClass();
    $res->joo_tramp = $f;
    $res->joo_args = $args;
    $res->joo_direct = $direct;
    return $res;
});

// CPS & Effects primitives
$GLOBALS['caml_current_stack'] = (object)['k' => 0, 'x' => 0, 'h' => 0, 'e' => 0];
$GLOBALS['caml_stack_depth'] = 40;

function caml_stack_check_depth() {
    return --$GLOBALS['caml_stack_depth'] > 0;
}
_jsoo_set_global('caml_stack_check_depth', 'caml_stack_check_depth');

function caml_push_trap($handler) {
    $GLOBALS['caml_current_stack']->x = (object)['h' => $handler, 't' => $GLOBALS['caml_current_stack']->x];
}
_jsoo_set_global('caml_push_trap', 'caml_push_trap');

function caml_pop_trap() {
    if (!$GLOBALS['caml_current_stack']->x) {
        return function ($x) { throw $x; };
    }
    $h = $GLOBALS['caml_current_stack']->x->h;
    $GLOBALS['caml_current_stack']->x = $GLOBALS['caml_current_stack']->x->t;
    return $h;
}
_jsoo_set_global('caml_pop_trap', 'caml_pop_trap');

$GLOBALS['caml_named_values'] = [];
_jsoo_set_global('caml_register_named_value', function($name, $v) {
    $GLOBALS['caml_named_values'][$name] = $v;
});

function caml_make_unhandled_effect_exn($eff) {
    $name = "Effect.Unhandled";
    $exn = isset($GLOBALS['caml_named_values'][$name]) ? $GLOBALS['caml_named_values'][$name] : null;
    if ($exn) return new CamlBlock([0, $exn, $eff]);
    return new CamlBlock([248, $name, caml_fresh_oo_id(0)]);
}

function caml_pop_fiber() {
    $c = $GLOBALS['caml_current_stack']->e;
    $GLOBALS['caml_current_stack']->e = 0;
    $GLOBALS['caml_current_stack'] = $c;
    return $c->k;
}

_jsoo_set_global('caml_perform_effect', function($eff, $k0) {
    if ($GLOBALS['caml_current_stack']->e === 0) {
        throw new CamlException(caml_make_unhandled_effect_exn($eff));
    }
    $handler = $GLOBALS['caml_current_stack']->h[3];
    $last_fiber = $GLOBALS['caml_current_stack'];
    $last_fiber->k = $k0;
    $cont = new CamlBlock([245, $last_fiber, $last_fiber]);
    $k1 = caml_pop_fiber();
    return caml_cps_call($handler, $eff, $cont, $last_fiber, $k1);
});

_jsoo_set_global('caml_reperform_effect', function($eff, $cont, $last, $k0) {
    if ($GLOBALS['caml_current_stack']->e === 0) {
        $exn = caml_make_unhandled_effect_exn($eff);
        $stack = caml_continuation_use_noexc($cont);
        caml_resume_stack($stack, $last, $k0);
        throw new CamlException($exn);
    }
    $handler = $GLOBALS['caml_current_stack']->h[3];
    $last_fiber = $GLOBALS['caml_current_stack'];
    $last_fiber->k = $k0;
    $last->e = $last_fiber;
    $cont[2] = $last_fiber;
    $k1 = caml_pop_fiber();
    return caml_cps_call($handler, $eff, $cont, $last_fiber, $k1);
});

function caml_resume_stack($stack, $last, $k) {
    if (!$stack) {
         $name = "Effect.Continuation_already_resumed";
         $exn = isset($GLOBALS['caml_named_values'][$name]) ? $GLOBALS['caml_named_values'][$name] : null;
         if (!$exn) $exn = new CamlBlock([248, $name, caml_fresh_oo_id(0)]);
         throw new CamlException($exn);
    }
    if ($last === 0) {
        $last = $stack;
        while ($last->e !== 0) $last = $last->e;
    }
    $GLOBALS['caml_current_stack']->k = $k;
    $last->e = $GLOBALS['caml_current_stack'];
    $GLOBALS['caml_current_stack'] = $stack;
    return caml_trampoline_return($stack->k, [$k], 0);
}
_jsoo_set_global('caml_resume_stack', 'caml_resume_stack');

_jsoo_set_global('caml_alloc_stack', function($hv, $hx, $hf) {
    $handlers = [0, $hv, $hx, $hf];
    return (object)[
        'k' => function($x) use ($handlers) {
            return caml_cps_call($handlers[1], $x, caml_pop_fiber());
        },
        'x' => (object)['h' => function($e) use ($handlers) {
            return caml_cps_call($handlers[2], $e, caml_pop_fiber());
        }, 't' => 0],
        'h' => $handlers,
        'e' => 0
    ];
});

function caml_continuation_use_noexc($cont) {
    $stack = $cont[1];
    $cont[1] = 0;
    return $stack;
}
_jsoo_set_global('caml_continuation_use_noexc', 'caml_continuation_use_noexc');

_jsoo_set_global('caml_cps_trampoline', function($f, $args) {
    $saved_stack_depth = $GLOBALS['caml_stack_depth'];
    $saved_current_stack = $GLOBALS['caml_current_stack'];
    try {
        $GLOBALS['caml_current_stack'] = (object)['k' => 0, 'x' => 0, 'h' => 0, 'e' => 0];
        if ($args instanceof CamlBlock) $args = $args->fields;
        $args[] = function ($x) { return $x; };
        $res = (object)['joo_tramp' => $f, 'joo_args' => $args, 'joo_direct' => 0];
        do {
            $GLOBALS['caml_stack_depth'] = 40;
            try {
                $res = (isset($res->joo_direct) && $res->joo_direct)
                    ? ($res->joo_tramp)(...$res->joo_args)
                    : caml_call_gen($res->joo_tramp, $res->joo_args);
            } catch (Throwable $e) {
                if (!$GLOBALS['caml_current_stack']->x) throw $e;
                $handler = $GLOBALS['caml_current_stack']->x->h;
                $GLOBALS['caml_current_stack']->x = $GLOBALS['caml_current_stack']->x->t;
                $res = (object)['joo_tramp' => $handler, 'joo_args' => [caml_wrap_exception($e)], 'joo_direct' => 1];
            }
        } while ($res instanceof stdClass && isset($res->joo_args));
    } finally {
        $GLOBALS['caml_stack_depth'] = $saved_stack_depth;
        $GLOBALS['caml_current_stack'] = $saved_current_stack;
    }
    return $res;
});

function caml_cps_call($f, ...$args) {
    if (caml_stack_check_depth()) {
        return caml_call_gen($f, $args);
    } else {
        return caml_trampoline_return($f, $args, 0);
    }
}
_jsoo_set_global('caml_exact_trampoline_cps_call', 'caml_cps_call');
_jsoo_set_global('caml_exact_trampoline_cps_call_0', 'caml_cps_call');
_jsoo_set_global('caml_exact_trampoline_cps_call_1', 'caml_cps_call');
_jsoo_set_global('caml_exact_trampoline_cps_call_2', 'caml_cps_call');
_jsoo_set_global('caml_exact_trampoline_cps_call_3', 'caml_cps_call');
_jsoo_set_global('caml_trampoline_cps_call1', 'caml_cps_call');
_jsoo_set_global('caml_trampoline_cps_call2', 'caml_cps_call');
_jsoo_set_global('caml_trampoline_cps_call3', 'caml_cps_call');
_jsoo_set_global('caml_trampoline_cps_call4', 'caml_cps_call');
_jsoo_set_global('caml_trampoline_cps_call5', 'caml_cps_call');
_jsoo_set_global('caml_trampoline_cps_call6', 'caml_cps_call');

_jsoo_set_global('caml_int32_format', function($fmt, $x) { return sprintf($fmt, $x); });
_jsoo_set_global('caml_nativeint_format', function($fmt, $x) { return sprintf($fmt, $x); });
_jsoo_set_global('caml_int64_format', function($fmt, $x) { return sprintf($fmt, $x); });

// Lazy primitives
_jsoo_set_global('caml_lazy_reset_to_lazy', function($lazy, $f = null) {
    $lazy[0] = 246; // Forcing
    if (func_num_args() > 1) $lazy[1] = $f;
    return 0;
});
_jsoo_set_global('caml_lazy_update_to_forcing', function($lazy, $f = null) {
    $lazy[0] = 246;
    if (func_num_args() > 1) $lazy[1] = $f;
    return 0;
});
_jsoo_set_global('caml_lazy_update_to_forward', function($lazy, $v = null) {
    $lazy[0] = 250; // Forward
    if (func_num_args() > 1) $lazy[1] = $v;
    return 0;
});
_jsoo_set_global('caml_alloc_dummy_lazy', function() { return new CamlBlock([246, function() { return 0; }]); });
_jsoo_set_global('caml_update_dummy_lazy', function($lazy, $v = null) {
    $lazy[0] = $v[0];
    $lazy[1] = $v[1];
    return 0;
});
_jsoo_set_global('caml_update_dummy', function($dummy, $v) {
    $dummy->fields = $v->fields;
    return 0;
});

function caml_equal($a, $b) {
    if ($a === $b) return 1;
    if ($a instanceof CamlBlock && $b instanceof CamlBlock) {
        if (count($a->fields) !== count($b->fields)) return 0;
        foreach ($a->fields as $i => $v) {
            if (!caml_equal($v, $b->fields[$i])) return 0;
        }
        return 1;
    }
    return $a == $b ? 1 : 0;
}
_jsoo_set_global('caml_equal', 'caml_equal');

function caml_compare($a, $b) {
    if ($a === $b) return 0;
    if ($a instanceof CamlBlock && $b instanceof CamlBlock) {
        $tag_a = $a->fields[0];
        $tag_b = $b->fields[0];
        if ($tag_a !== $tag_b) return $tag_a <=> $tag_b;
        $ca = count($a->fields);
        $cb = count($b->fields);
        if ($ca !== $cb) return $ca <=> $cb;
        for ($i = 1; $i < $ca; $i++) {
            $res = caml_compare($a->fields[$i], $b->fields[$i]);
            if ($res !== 0) return $res;
        }
        return 0;
    }
    if ($a instanceof CamlBlock || $b instanceof CamlBlock) {
        return ($a instanceof CamlBlock) ? 1 : -1;
    }
    return $a <=> $b;
}
_jsoo_set_global('caml_compare', 'caml_compare');

_jsoo_set_global('caml_array_concat', function($l) {
    $res = [0];
    while ($l instanceof CamlBlock && $l->fields[0] === 0) {
        $arr = $l->fields[1];
        if ($arr instanceof CamlBlock) {
            for ($i = 1; $i < count($arr->fields); $i++) $res[] = $arr->fields[$i];
        }
        $l = $l->fields[2];
    }
    return new CamlBlock($res);
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

// Bitwise shift right unsigned
function caml_shift_right_unsigned($a, $b) {
    $a = (int)$a; $b = (int)$b;
    if ($b >= 32) return 0;
    return ($a & 0xFFFFFFFF) >> $b;
}
_jsoo_set_global('caml_shift_right_unsigned', 'caml_shift_right_unsigned');

function caml_int32($x) {
    return unpack("l", pack("l", (int)$x))[1];
}
_jsoo_set_global('caml_int32', 'caml_int32');

function caml_call_gen($f, $args) {
    if ($args instanceof CamlBlock) $args = $args->fields;
    if (is_array($args)) $args = array_values($args);
    else return $f($args);
    
    $n = count($args);
    if ($n === 0) return $f;
    $arity = caml_get_arity($f);
    if ($arity <= 0) return $f(...$args);
    if ($arity === $n) return $f(...$args);
    if ($arity < $n) {
        $res = $f(...array_slice($args, 0, $arity));
        return caml_call_gen($res, array_slice($args, $arity));
    }
    return function(...$extra_args) use ($f, $args) {
        return caml_call_gen($f, array_merge($args, $extra_args));
    };
}
_jsoo_set_global('caml_call_gen', 'caml_call_gen');
_jsoo_set_global('caml_call_gen_cps', 'caml_call_gen');

_jsoo_set_global('caml_call1', function($f, ...$args) { return caml_call_gen($f, $args); });
_jsoo_set_global('caml_call2', function($f, ...$args) { return caml_call_gen($f, $args); });
_jsoo_set_global('caml_call3', function($f, ...$args) { return caml_call_gen($f, $args); });
_jsoo_set_global('caml_call4', function($f, ...$args) { return caml_call_gen($f, $args); });
_jsoo_set_global('caml_call5', function($f, ...$args) { return caml_call_gen($f, $args); });
