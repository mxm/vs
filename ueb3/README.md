#Aufgabe 3.1

##1.

Wieviele Threads werden standardmäßig bei Ihnen genutzt und was für ein
System (Prozessor) nutzen Sie?

Es werden standardmäßig 2 Threads von OpenMP benutzt.

Es wir d ein Intel(R) Core(TM)2 CPU mit zwei 2.00GHz Kernen verwendet.

    processor: 0
    vendor_id: GenuineIntel
    cpu family: 6
    model: 15
    model name: Intel(R) Core(TM)2 CPU         T7200  @ 2.00GHz
    stepping: 6
    cpu MHz: 1000.000
    cache size: 4096 KB
    physical id: 0
    siblings: 2
    core id: 0
    cpu cores: 2
    apicid: 0
    initial apicid: 0
    fdiv_bug: no
    hlt_bug: no
    f00f_bug: no
    coma_bug: no
    fpu: yes
    fpu_exception: yes
    cpuid level: 10
    wp: yes
    flags: fpu vme de pse tsc msr pae mce cx8 apic mtrr pge mca cmov pat pse36
    clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe nx lm constant_tsc
    arch_perfmon pebs bts aperfmperf pni dtes64 monitor ds_cpl vmx est tm2
    ssse3 cx16 xtpr pdcm lahf_lm dts tpr_shadow
    bogomips: 3989.92
    clflush size: 64
    cache_alignment: 64
    address sizes: 36 bits physical, 48 bits virtual
    power management:

    processor: 1
    vendor_id: GenuineIntel
    cpu family: 6
    model: 15
    model name: Intel(R) Core(TM)2 CPU         T7200  @ 2.00GHz
    stepping: 6
    cpu MHz: 1000.000
    cache sizes: 4096 KB
    physical id: 0
    siblings: 2
    core id: 1
    cpu cores: 2
    apicid: 1
    initial apicid: 1
    fdiv_bug: no
    hlt_bug: no
    f00f_bug: no
    coma_bug: no
    fpu_exception: yes
    fpu_exception: yes
    cpuid level: 10
    wp: yes
    flags: fpu vme de pse tsc msr pae mce cx8 apic mtrr pge mca cmov pat pse36
    clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe nx lm constant_tsc
    arch_perfmon pebs bts aperfmperf pni dtes64 monitor ds_cpl vmx est tm2
    ssse3 cx16 xtpr pdcm lahf_lm dts tpr_shadow
    bogomips: 3989.75
    clflush sizes: 64
    cache_alignment: 64
    address sizes: 36 bits physical, 48 bits virtual

##2.

Beobachten Sie Ihr Program mit unterschiedlich vielen Threads und
Array-Größen und erörtern Sie die Laufzeit.

    OMP_NUM_THREADS=1 ./sum 1024
    Using 1 threads in parallel sections.
    Time: 6
    OMP_NUM_THREADS=1 ./sum 2048
    Using 1 threads in parallel sections.
    Time: 11
    OMP_NUM_THREADS=1 ./sum 4096
    Using 1 threads in parallel sections.
    Time: 19
    OMP_NUM_THREADS=1 ./sum 8192
    Using 1 threads in parallel sections.
    Time: 37
    OMP_NUM_THREADS=2 ./sum 1024
    Using 2 threads in parallel sections.
    Time: 7
    OMP_NUM_THREADS=2 ./sum 2048
    Using 2 threads in parallel sections.
    Time: 9
    OMP_NUM_THREADS=2 ./sum 4096
    Using 2 threads in parallel sections.
    Time: 13
    OMP_NUM_THREADS=2 ./sum 8192
    Using 2 threads in parallel sections.
    Time: 22
    OMP_NUM_THREADS=4 ./sum 1024
    Using 4 threads in parallel sections.
    Time: 47
    OMP_NUM_THREADS=4 ./sum 2048
    Using 4 threads in parallel sections.
    Time: 54
    OMP_NUM_THREADS=4 ./sum 4096
    Using 4 threads in parallel sections.
    Time: 56
    OMP_NUM_THREADS=4 ./sum 8192
    Using 4 threads in parallel sections.
    Time: 67
    OMP_NUM_THREADS=6 ./sum 1024
    Using 6 threads in parallel sections.
    Time: 64
    OMP_NUM_THREADS=6 ./sum 2048
    Using 6 threads in parallel sections.
    Time: 73
    OMP_NUM_THREADS=6 ./sum 4096
    Using 6 threads in parallel sections.
    Time: 82
    OMP_NUM_THREADS=6 ./sum 8192
    Using 6 threads in parallel sections.
    Time: 89

Bei einem Thread verhält sich die Laufzeit fast linear zur Länge des Arrays. Auf
einem System mit zwei Kernen und zwei Threads können wir die Berechnung für
große Threads leicht beschleunigen. Bei mehr Threads als Kernen ist der Overhead
für die Threadverwaltung zu groß, sodass wir unnötig CPU-Zyklen verschwenden und
das Programm sogar langsamer wird.

#Aufgabe 3.2
