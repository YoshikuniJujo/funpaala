xxcrypt n ed mc = mc ^ ed `mod` n

encrypt = xxcrypt 138689 13
decrypt = xxcrypt 138689 95497
