#!/usr/bin/env python

from setuptools import setup

setup(
    name='Lib',
    packages=['lib'],
    install_requires=[
        'requests==2.21.0',
        'simplejson==3.16.0',
        'pywallet==0.1.0',
        'eth-account==0.5.9',
    ],
    extras_require={
        'dev': [
            'pytest',
            'python-dotenv',
        ],
    },
)
