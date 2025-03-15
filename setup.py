from setuptools import setup, find_packages

setup(
    name="toy_lang_fun",
    version="0.1.2",
    author="Krishna Murugan",
    description="A toy functional programming language",
    py_modules=["main"],
    entry_points={
        "console_scripts": [
            "toy_lang=main:main"
        ]
    }
)