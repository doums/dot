function cash --description 'Compare SHA256 hash of a file'
    if test (count $argv) -lt 2
        echo "Usage: cash <file> <expected_hash>"
        return 1
    end

    set file $argv[1]
    set expected (string upper (string trim $argv[2]))

    if not test -f $file
        echo (set_color red)"Error: File not found: $file"(set_color normal)
        return 1
    end

    set hash (sha256sum $file | string upper | string split ' ')[1]

    echo "File:     $(basename $file)"
    echo "Hash:     $hash"
    echo "Expected: $expected"
    if test "$hash" = "$expected"
        echo (set_color -o brgreen)"✓ Match"(set_color normal)
    else
        echo (set_color -o red)"✗ Mismatch"(set_color normal)
    end
end
