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

class CamlBlock implements ArrayAccess, Countable, IteratorAggregate {
    public array $fields;
    public function __construct(array $fields) {
        $this->fields = $fields;
    }
    public function offsetExists($offset): bool { return array_key_exists($offset, $this->fields); }
    #[\ReturnTypeWillChange]
    public function &offsetGet($offset) { return $this->fields[$offset]; }
    public function offsetSet($offset, $value): void { $this->fields[$offset] = $value; }
    public function offsetUnset($offset): void { unset($this->fields[$offset]); }
    public function count(): int { return count($this->fields); }
    public function getIterator(): Traversable { return new ArrayIterator($this->fields); }
}

$GLOBALS['caml_arities'] = [];
function caml_set_arity($f, $arity) {
    if (is_object($f) && !($f instanceof CamlBlock)) {
        $GLOBALS['caml_arities'][spl_object_hash($f)] = $arity;
    }
}

function caml_get_arity($f) {
    if (!is_object($f) || ($f instanceof CamlBlock)) return -1;
    $hash = spl_object_hash($f);
    if (isset($GLOBALS['caml_arities'][$hash])) return $GLOBALS['caml_arities'][$hash];
    if ($f instanceof Closure) {
        $ref = new ReflectionFunction($f);
        return $ref->getNumberOfParameters();
    }
    return -1;
}

function caml_call_gen($f, $args) {
    if ($args instanceof CamlBlock) $args = $args->fields;
    $n = count($args);
    if ($n === 0) return $f;
    $arity = caml_get_arity($f);
    if ($arity <= 0 || $arity === $n) return $f(...$args);
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
function caml_int32($x) { return unpack("l", pack("l", (int)$x))[1]; }
function caml_typeof($x) {
    if (is_int($x) || is_float($x)) return "number";
    if (is_string($x)) return "string";
    return "object";
}
_jsoo_set_global('caml_int32', 'caml_int32');
_jsoo_set_global('caml_typeof', 'caml_typeof');

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
        $this->fiber = new Fiber(function($v) use ($func) {
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
        $cont = new CamlBlock([0, $this, 0]);
        // Le wrapper effc d'OCaml gère le match et le reperform éventuel
        return caml_call_gen($this->handlers[3], [$eff, $cont, 0]);
    }
}

function caml_wrap_exception($e) {
    if ($e instanceof CamlException) return $e->caml_val;
    return new CamlBlock([0, 0, $e->getMessage()]);
}

function caml_perform_effect($eff) {
    if ($GLOBALS['caml_current_stack']->e === 0) {
        $name = "Effect.Unhandled";
        $exn = new CamlBlock([248, $name, $GLOBALS['caml_oo_last_id']++]);
        throw new CamlException(new CamlBlock([0, $exn, $eff]));
    }
    return Fiber::suspend($eff);
}

_jsoo_set_global('caml_perform_effect', 'caml_perform_effect');
_jsoo_set_global('caml_reperform_effect', function($eff, $stack, $last) { return caml_perform_effect($eff); });
_jsoo_set_global('caml_alloc_stack', function($hv, $hx, $hf) {
    return (object)['fresh' => true, 'handlers' => [0, $hv, $hx, $hf]];
});

_jsoo_set_global('caml_resume_stack', function($stack, $last, $v) {
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
_jsoo_set_global('caml_js_length', function($x) { return is_array($x) ? count($x) : 0; });
_jsoo_set_global('caml_seq', function($e1, $e2) { return $e2; });
_jsoo_set_global('caml_maybe_attach_backtrace', function($v) { return $v; });
_jsoo_set_global('caml_fresh_oo_id', function() { return $GLOBALS['caml_oo_last_id']++; });
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
_jsoo_set_global('caml_obj_dup', function($x) { return is_object($x) ? clone $x : $x; });
_jsoo_set_global('caml_obj_tag', function($x) { return ($x instanceof CamlBlock) ? $x->fields[0] : 1000; });
_jsoo_set_global('caml_ml_string_length', 'strlen');
_jsoo_set_global('caml_ml_bytes_length', 'strlen');
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

// Wrappers d'appels
for($i=1; $i<=5; $i++) {
    _jsoo_set_global("caml_call$i", function($f, ...$args) { return caml_call_gen($f, $args); });
}
