import jupyter_kernel_test as jkt

class ElixirKernelTests(jkt.KernelTests):
    kernel_name = "elixir_test"

    language_name = "elixir"

    file_extension = ".ex"

    code_hello_world = 'IO.puts("hello, world")'

    completion_samples = [
        {
            'text': 'Enum.a',
            'matches': {
                'all?',
                'any?',
                'at',
            },
        },
    ]

    complete_code_samples = [
        '1',
        ":io.format('asdf')",
        "func = fn (a, b) -> a * b end\nfunc.(2, 3)",
        ":lists.reverse([1,2,3])",
    ]

    incomplete_code_samples = ["IO.puts(", "func = fn () ->"]

    code_generate_error = 'raise "oops"'

    code_execute_result = [
        {'code': "1+2+3", 'result': "6"}
    ]

    code_display_data = [
    ]


if __name__ == '__main__':
    import unittest
    unittest.main()
