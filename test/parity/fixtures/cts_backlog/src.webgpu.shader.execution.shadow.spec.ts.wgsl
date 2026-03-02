diagnostic(warning, shadowing);

struct S {
        my_var_start: u32,
        my_var_block_shadow: u32,
        my_var_unshadow: u32,
        my_var_param_shadow: u32,
        my_var_param_reshadow: u32,
        my_var_after_func: u32,

        my_const_start: u32,
        my_const_block_shadow: u32,
        my_const_unshadow: u32,
        my_const_param_shadow: u32,
        my_const_param_reshadow: u32,
        my_const_after_func: u32,

        my_let_block_shadow: u32,
        my_let_param_reshadow: u32,
        my_let_after_func: u32,

        my_func_param_shadow: u32,
        my_func_shadow: u32,
      }
      @group(0) @binding(0) var<storage, read_write> buffer : S;

      var<private> my_var: u32  = 1;
      const my_const: u32 = 100;

      @compute @workgroup_size(1)
      fn main() {
        let my_let = 200u;

        buffer.my_var_start = my_var;  // 1
        buffer.my_const_start = my_const;  // 100

        if (true) {
            var my_var: u32 = 10u;
            let my_const: u32 = 110u;

            buffer.my_var_block_shadow = my_var;  // 10
            buffer.my_const_block_shadow = my_const;  // 110

            let my_let = 210u;
            buffer.my_let_block_shadow = my_let;  // 210
        }

        buffer.my_var_unshadow = my_var;  // 1
        buffer.my_const_unshadow = my_const;  // 100

        my_func(20, 120, my_let, 300);

        buffer.my_var_after_func = my_var;  // 1
        buffer.my_const_after_func = my_const;  // 100
        buffer.my_let_after_func = my_let;  // 200;
      }

      // Note, defined after |main|
      fn my_func(my_var: u32, my_const: u32, my_let: u32, my_func: u32) {
        buffer.my_var_param_shadow = my_var;  // 20
        buffer.my_const_param_shadow = my_const;  // 120

        buffer.my_func_param_shadow = my_func; // 300

        // Need a nested scope to test parameter shadowing behavior.
        if (true) {
          var my_var = 30u;
          let my_const = 130u;

          buffer.my_var_param_reshadow = my_var; // 30
          buffer.my_const_param_reshadow = my_const; // 130

          let my_let = 220u;
          buffer.my_let_param_reshadow = my_let; // 220

          let my_func: u32 = 310u;
          buffer.my_func_shadow = my_func;  // 310
        }
      }
