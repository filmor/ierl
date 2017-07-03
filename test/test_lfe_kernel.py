import unittest
import jupyter_kernel_test as jkt

class LfeKernelTests(jkt.KernelTests):
    kernel_name = "lfe_test"

    language_name = "lfe"

    file_extension = ".lfe"

    code_hello_world = '(io:format "hello, world~n")'

    completion_samples = [
        {
            'text': '(lists:s',
            'matches': {
                'split',
                'sublist',
                'sort',
                'subtract',
                'splitwith',
                'sum',
                'suffix',
                'seq',
            },
        },
    ]

    complete_code_samples = [
        '1',
        '(io:format "asdf")',
        "(lists:reverse (1 2 3))",
    ]

    incomplete_code_samples = ["(io:format "]

    code_generate_error = "(error blubb)"

    code_execute_result = [
        {'code': "(+ 1 2 3)", 'result': "6"}
    ]

    code_display_data = [
    ]


if __name__ == '__main__':
    unittest.main()
