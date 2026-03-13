<?php
// Ocamlephan Runtime - Support OCaml 5 Fibers (V4 stable)
set_time_limit(10);
error_reporting(E_ALL & ~E_WARNING & ~E_NOTICE & ~E_DEPRECATED);

class CamlException extends Exception {
    public $caml_val;
    public function __construct($v) {
        parent::__construct("OCaml Exception");
        $this->caml_val = $v;
    }
}

$GLOBALS['caml_exceptions_registry'] = [];
function caml_block($fields) {
    if (isset($fields[0]) && $fields[0] === 248 && isset($fields[1]) && is_string($fields[1])) {
        $name = $fields[1];
        if (isset($GLOBALS['caml_exceptions_registry'][$name])) {
             return $GLOBALS['caml_exceptions_registry'][$name];
        }
        $b = new CamlBlock($fields);
        $GLOBALS['caml_exceptions_registry'][$name] = $b;
        return $b;
    }
    return new CamlBlock($fields);
}
_jsoo_set_global('caml_block', function($f) { return caml_block($f); });

class CamlBlock implements ArrayAccess, Countable, IteratorAggregate {
    public array $fields;
    public function __construct(array $fields) {
        $this->fields = $fields;
    }
    public function offsetExists($offset): bool { return array_key_exists($offset, $this->fields); }
    #[\ReturnTypeWillChange]
    public function &offsetGet($offset) { 
        if (!array_key_exists($offset, $this->fields)) {
            $null = null;
            return $null;
        }
        return $this->fields[$offset]; 
    }
    public function offsetSet($offset, $value): void { $this->fields[$offset] = $value; }
    public function offsetUnset($offset): void { unset($this->fields[$offset]); }
    public function count(): int { return count($this->fields); }
    public function getIterator(): Traversable { return new ArrayIterator($this->fields); }
}

$GLOBALS['caml_arities'] = new WeakMap();
function caml_set_arity($f, $arity) {
    if (is_object($f) && !($f instanceof CamlBlock)) {
        $GLOBALS['caml_arities'][$f] = $arity;
    }
}

function caml_get_arity($f) {
    if (!is_object($f)) {
        if (is_string($f) && is_callable($f)) {
            return 1;
        }
        return -1;
    }
    if ($f instanceof CamlBlock) {
        if (isset($f->fields[1]) && (is_callable($f->fields[1]) || $f->fields[1] instanceof CamlBlock)) {
            return caml_get_arity($f->fields[1]);
        }
        return -1;
    }
    if (isset($GLOBALS['caml_arities'][$f])) return $GLOBALS['caml_arities'][$f];
    if ($f instanceof Closure || is_callable($f)) {
        try {
            $ref = new ReflectionFunction($f);
            $arity = $ref->getNumberOfRequiredParameters();
            $p_total = $ref->getNumberOfParameters();
            if ($arity === 0 && $p_total > 0) $arity = 1;
            if ($arity === 0) $arity = 1;
            $GLOBALS['caml_arities'][$f] = $arity;
            return $arity;
        } catch (Exception $e) { return 1; }
    }
    return -1;
}

function caml_call_gen($f, $args) {
    if ($args instanceof CamlBlock) $args = array_slice($args->fields, 0);
    if (!is_array($args)) $args = [$args];
    else $args = array_values($args);

    $n = count($args);
    if ($n === 0) return $f;
    
    if ($f instanceof CamlBlock) {
        if (isset($f->fields[1]) && (is_callable($f->fields[1]) || $f->fields[1] instanceof CamlBlock)) {
            return caml_call_gen($f->fields[1], $args);
        }
    }

    $arity = caml_get_arity($f);
    if ($arity > $n) {
        return function(...$extra) use ($f, $args) {
            return caml_call_gen($f, array_merge($args, $extra));
        };
    }
    
    if ($arity <= 0 || $arity === $n) {
        try {
            return $f(...$args);
        } catch (Throwable $e) {
            throw $e;
        }
    }
    if ($arity < $n) {
        $res = $f(...array_slice($args, 0, $arity));
        return caml_call_gen($res, array_slice($args, $arity));
    }
    return function(...$extra) use ($f, $args) {
        return caml_call_gen($f, array_merge($args, $extra));
    };
}

function _jsoo_set_global($name, $value) {
    $GLOBALS[$name] = $value;
    if (!isset($GLOBALS['globalThis'])) $GLOBALS['globalThis'] = new stdClass();
    $GLOBALS['globalThis']->$name = $value;
}

// Primitives de base
_jsoo_set_global('caml_call_gen', 'caml_call_gen');

_jsoo_set_global('_jsoo_call_main', function($main, $global) {
    try {
        return $main($global);
    } catch (CamlException $e) {
        $v = $e->caml_val;
        if ($v instanceof CamlBlock && $v->fields[0] === 0 && isset($v->fields[1])) {
            $exn = $v->fields[1];
            // If it's Undefined_recursive_module and we are at top-level
            if ($exn instanceof CamlBlock && $exn->fields[0] === 248 && $exn->fields[1] === "Undefined_recursive_module") {
                $file = $v->fields[2]->fields[1];
                $line = $v->fields[2]->fields[2];
                $col = $v->fields[2]->fields[3];
                fwrite(STDERR, "Fatal error: exception Undefined_recursive_module(\"$file\", $line, $col)\n");
                exit(2);
            }
        }
        throw $e;
    }
});
function caml_int32($x) { return unpack("l", pack("l", (int)$x))[1]; }
_jsoo_set_global('caml_int32', 'caml_int32');

function caml_typeof($x) {
    if (is_int($x) || is_float($x)) return "number";
    if (is_string($x)) return "string";
    if (is_bool($x)) return "boolean";
    return "object";
}
_jsoo_set_global('caml_typeof', 'caml_typeof');

function caml_js_length($x) { 
    if ($x instanceof CamlBlock) return count($x->fields);
    if (is_array($x)) return count($x);
    if (is_string($x)) return strlen($x);
    return 0; 
}
_jsoo_set_global('caml_js_length', 'caml_js_length');

function caml_seq($e1, $e2) { return $e2; }
_jsoo_set_global('caml_seq', 'caml_seq');

function caml_shift_right_unsigned($a, $b) {
    $a = (int)$a; $b = (int)$b;
    if ($b >= 32) return 0;
    return ($a & 0xFFFFFFFF) >> $b;
}
_jsoo_set_global('caml_shift_right_unsigned', 'caml_shift_right_unsigned');

// Trampoline
function caml_trampoline($res) {
    while ($res instanceof stdClass && isset($res->joo_tramp)) {
        $res = ($res->joo_tramp)(...$res->joo_args);
    }
    return $res;
}
_jsoo_set_global('caml_trampoline', 'caml_trampoline');

function caml_trampoline_return($f, $args, $direct) {
    $res = new stdClass();
    $res->joo_tramp = $f;
    $res->joo_args = $args;
    $res->joo_direct = $direct;
    return $res;
}
_jsoo_set_global('caml_trampoline_return', 'caml_trampoline_return');

// Support des Effets via Fibers PHP 8.1+
$GLOBALS['caml_current_stack'] = (object)['k' => 0, 'x' => 0, 'h' => 0, 'e' => 0];
$GLOBALS['caml_oo_last_id'] = 0;

class CamlFiber {
    public $fiber;
    public $stack;
    public $handlers;

    public function __construct($func, $handlers) {
        $this->handlers = $handlers;
        $this->stack = (object)[
            'k' => $this,
            'x' => 0,
            'h' => $handlers,
            'e' => $GLOBALS['caml_current_stack']
        ];
        $this->fiber = new Fiber(function($v = 0) use ($func) {
            $saved = $GLOBALS['caml_current_stack'];
            $GLOBALS['caml_current_stack'] = $this->stack;
            try {
                $res = caml_call_gen($func, [$v]);
                $GLOBALS['caml_current_stack'] = $saved;
                // retc : handlers[1]
                return caml_call_gen($this->handlers[1], [$res]);
            } catch (Throwable $e) {
                $GLOBALS['caml_current_stack'] = $saved;
                // exnc : handlers[2]
                return caml_call_gen($this->handlers[2], [caml_wrap_exception($e)]);
            }
        });
    }

    public function run($v = 0) { return $this->handle($this->fiber->start($v)); }
    public function resume($v) { 
        if ($this->fiber->isTerminated()) return $v;
        if (!$this->fiber->isStarted()) return $this->run($v);
        return $this->handle($this->fiber->resume($v)); 
    }

    private function handle($val) {
        if ($this->fiber->isTerminated()) return $this->fiber->getReturn();
        if (!$this->fiber->isSuspended()) return $val;
        
        $eff = $val;
        $k = new CamlBlock([0, $this, 0]);
        return caml_call_gen($this->handlers[3], [$eff, $k, 0]);
    }
}

function caml_wrap_exception($e) {
    if ($e instanceof CamlException) {
         return $e->caml_val;
    }
    if ($e instanceof Throwable) return new CamlBlock([0, 0, $e->getMessage()]);
    return $e;
}
_jsoo_set_global('caml_wrap_exception', function($e) { return caml_wrap_exception($e); });

function caml_perform_effect($eff) {
    if ($GLOBALS['caml_current_stack']->e === 0) {
        $name = "Effect.Unhandled";
        $exn = new CamlBlock([248, $name, $GLOBALS['caml_oo_last_id']++]);
        throw new CamlException(new CamlBlock([0, $exn, $eff]));
    }
    return Fiber::suspend($eff);
}

_jsoo_set_global('caml_perform_effect', function($eff) { return caml_perform_effect($eff); });
_jsoo_set_global('caml_reperform_effect', function($eff, $stack, $last) { return caml_perform_effect($eff); });
_jsoo_set_global('caml_alloc_stack', function($hv, $hx, $hf) {
    return (object)['fresh' => true, 'handlers' => [0, $hv, $hx, $hf]];
});

_jsoo_set_global('caml_resume_stack', function($stack, $last, $v) {
    if ($stack instanceof CamlFiber) return $stack->resume($v);
    if (isset($stack->fresh)) {
        $cf = new CamlFiber($last, $stack->handlers);
        return $cf->resume($v); 
    }
    return $stack->resume($v);
});

_jsoo_set_global('caml_continuation_use_noexc', function($cont) {
    if ($cont instanceof CamlBlock) {
        $stack = $cont->fields[1];
        $cont->fields[1] = 0;
        return $stack;
    }
    return $cont;
});

// Primitives manquantes
_jsoo_set_global('caml_fs_init', function() {});
_jsoo_set_global('caml_register_global', function($n, $v, $name) { $GLOBALS[$name] = $v; });
_jsoo_set_global('caml_register_named_value', function($name, $v) {});
_jsoo_set_global('caml_maybe_attach_backtrace', function($v) { return $v; });
_jsoo_set_global('caml_fresh_oo_id', function() { return $GLOBALS['caml_oo_last_id']++; });
_jsoo_set_global('caml_set_oo_id', function($b) { $b[2] = $GLOBALS['caml_oo_last_id']++; return $b; });
_jsoo_set_global('caml_bytes_of_string', function($s) { return $s; });
_jsoo_set_global('caml_ml_open_descriptor_in', function($d) { return $d; });
_jsoo_set_global('caml_ml_open_descriptor_out', function($d) { return $d; });
_jsoo_set_global('caml_sys_executable_name', function() { return "ocaml"; });
_jsoo_set_global('caml_sys_get_config', function() { return new CamlBlock([0, "php", 32]); });
_jsoo_set_global('caml_sys_const_ostype_unix', function() { return 1; });
_jsoo_set_global('caml_sys_const_ostype_win32', function() { return 0; });
_jsoo_set_global('caml_sys_const_ostype_cygwin', function() { return 0; });
_jsoo_set_global('caml_sys_const_max_wosize', function() { return 0x7FFFFFFF; });
_jsoo_set_global('caml_sys_io_buffer_size', function() { return 4096; });
_jsoo_set_global('caml_create_bytes', function($n) { return str_repeat("\0", $n); });
_jsoo_set_global('caml_fill_bytes', function(&$s, $i, $n, $v) {
    for($j=0; $j<$n; $j++) $s[$i+$j] = chr($v);
});
_jsoo_set_global('caml_bytes_set', function(&$b, $i, $c) { $b[$i] = chr($c); });
_jsoo_set_global('caml_bytes_get', function($s, $i) { return ord($s[$i]); });
_jsoo_set_global('caml_blit_bytes', function($s, $si, &$d, $di, $len) {
    for ($i = 0; $i < $len; $i++) $d[$di + $i] = $s[$si + $i];
});
_jsoo_set_global('caml_obj_dup', function($x) { return is_object($x) ? clone $x : $x; });
_jsoo_set_global('caml_obj_tag', function($x) { return ($x instanceof CamlBlock) ? $x->fields[0] : 1000; });
_jsoo_set_global('caml_obj_block', function($tag, $len) { return new CamlBlock(array_merge([$tag], array_fill(0, $len, 0))); });
_jsoo_set_global('caml_ml_string_length', function($s) { return strlen($s); });
_jsoo_set_global('caml_ml_bytes_length', function($s) { return strlen($s); });
_jsoo_set_global('caml_string_get', function($s, $i) { return ord($s[$i]); });
_jsoo_set_global('caml_ml_output', function($ch, $s, $i, $l) { echo substr($s, $i, $l); });
_jsoo_set_global('caml_ml_output_char', function($ch, $c) { echo chr($c); });
_jsoo_set_global('caml_ml_flush', function($ch) { flush(); });
_jsoo_set_global('caml_atomic_cas_field', function(&$obj, $i, $old, $new) {
    if ($obj->fields[$i] === $old) { $obj->fields[$i] = $new; return 1; }
    return 0;
});
_jsoo_set_global('caml_atomic_fetch_add_field', function(&$obj, $i, $v) {
    $old = $obj->fields[$i]; $obj->fields[$i] += $v; return $old;
});
_jsoo_set_global('caml_ml_out_channels_list', function() { return 0; });
_jsoo_set_global('caml_check_bound', function($arr, $i) { return $arr; });
_jsoo_set_global('caml_check_bound_gen', function($arr, $i) { return $arr; });
_jsoo_set_global('caml_array_make', function($len, $v) { return new CamlBlock(array_merge([0], array_fill(0, $len, $v))); });
_jsoo_set_global('caml_array_blit', function($s, $si, $d, $di, $len) {
    for ($i = 0; $i < $len; $i++) $d[$di + $i + 1] = $s[$si + $i + 1];
});
_jsoo_set_global('caml_array_concat', function($arrs) {
    $res = [0];
    foreach ($arrs as $i => $arr) {
        if ($i === 0) continue;
        for ($j = 1; $j < count($arr->fields); $j++) {
            $res[] = $arr->fields[$j];
        }
    }
    return new CamlBlock($res);
});
_jsoo_set_global('caml_int_compare', function($a, $b) { return $a <=> $b; });
_jsoo_set_global('caml_string_compare', 'strcmp');
_jsoo_set_global('caml_equal_exn', function($a, $b) {
    if ($a === $b) return 1;
    if ($a instanceof CamlBlock && $b instanceof CamlBlock) {
         if ($a->fields[0] === 248 && $b->fields[0] === 248) {
              return $a->fields[1] === $b->fields[1] ? 1 : 0;
         }
    }
    return 0;
});

// Structural comparison
_jsoo_set_global('caml_equal', function($a, $b) {
    if ($a === $b) return 1;
    if ($a instanceof CamlBlock && $b instanceof CamlBlock) {
        return $a->fields == $b->fields ? 1 : 0;
    }
    return $a == $b ? 1 : 0;
});
_jsoo_set_global('caml_notequal', function($a, $b) { 
    if ($a === $b) return 0;
    if ($a instanceof CamlBlock && $b instanceof CamlBlock) {
        return $a->fields != $b->fields ? 1 : 0;
    }
    return $a != $b ? 1 : 0;
});
_jsoo_set_global('caml_compare', function($a, $b) { return $a <=> $b; });
_jsoo_set_global('caml_greaterthan', function($a, $b) { return $a > $b ? 1 : 0; });
_jsoo_set_global('caml_greaterequal', function($a, $b) { return $a >= $b ? 1 : 0; });
_jsoo_set_global('caml_lessthan', function($a, $b) { return $a < $b ? 1 : 0; });
_jsoo_set_global('caml_lessequal', function($a, $b) { return $a <= $b ? 1 : 0; });

// Lazy primitives
_jsoo_set_global('caml_lazy_reset_to_lazy', function($lazy, $f = null) {
    $lazy[0] = 246; // Forcing
    if ($f !== null) $lazy[1] = $f;
    return 0;
});
_jsoo_set_global('caml_lazy_update_to_forcing', function($lazy, $f = null) {
    $lazy[0] = 246;
    if ($f !== null) $lazy[1] = $f;
    return 0;
});
_jsoo_set_global('caml_lazy_update_to_forward', function($lazy, $v = null) {
    $lazy[0] = 250; // Forward
    if ($v !== null) $lazy[1] = $v;
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

// Objects
$GLOBALS['caml_method_cache'] = [];
_jsoo_set_global('caml_oo_cache_id', function() {
    $id = count($GLOBALS['caml_method_cache']);
    $GLOBALS['caml_method_cache'][] = 0;
    return $id;
});

_jsoo_set_global('caml_get_public_method', function($obj, $tag) {
    if (!($obj instanceof CamlBlock)) {
        fwrite(STDERR, "FATAL: obj is not a block in get_public_method: "); var_dump($obj);
    }
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
    if (!($obj instanceof CamlBlock)) {
        fwrite(STDERR, "FATAL: obj is not a block in get_cached_method: "); var_dump($obj);
    }
    $meths = $obj[1];
    if (!($meths instanceof CamlBlock)) return 0;
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

// Wrappers d'appels
for($i=1; $i<=6; $i++) {
    _jsoo_set_global("caml_call$i", function($f, ...$args) { return caml_call_gen($f, $args); });
}
_jsoo_set_global('caml_format_float', function($fmt, $x) { return sprintf($fmt, $x); });
