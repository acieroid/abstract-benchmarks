;; Expected result: "public class BOut extends RuntimeEnvironment {\n public static void main (String[] args) {\nnew IntValue(3) ;\n }\n}\n"
(letrec ((cadr (lambda (p) (let ((_30 (cdr p))) (let ((_31 (car _30))) _31))))) (letrec ((caadr (lambda (p) (let ((_34 (cdr p))) (let ((_32 _34)) (let ((_35 (car _32))) (let ((_33 _35)) (let ((_36 (car _33))) _36)))))))) (letrec ((caddr (lambda (p) (let ((_39 (cdr p))) (let ((_37 _39)) (let ((_40 (cdr _37))) (let ((_38 _40)) (let ((_41 (car _38))) _41)))))))) (letrec ((cadddr (lambda (p) (let ((_47 (cdr p))) (let ((_44 _47)) (let ((_42 _44)) (let ((_48 (cdr _42))) (let ((_45 _48)) (let ((_43 _45)) (let ((_49 (cdr _43))) (let ((_46 _49)) (let ((_50 (car _46))) _50)))))))))))) (letrec ((map (lambda (f lst) (let ((_51 (pair? lst))) (if _51 (let ((_56 (car lst))) (let ((_52 _56)) (let ((_57 (f _52))) (let ((_53 _57)) (let ((_58 (cdr lst))) (let ((_54 _58)) (let ((_59 (map f _54))) (let ((_55 _59)) (let ((_60 (cons _53 _55))) _60))))))))) (quote ())))))) (letrec ((append (lambda (lst1 lst2) (let ((_61 (pair? lst1))) (let ((_62 (not _61))) (if _62 lst2 (let ((_65 (car lst1))) (let ((_66 (cdr lst1))) (let ((_63 _66)) (let ((_67 (append _63 lst2))) (let ((_64 _67)) (let ((_68 (cons _65 _64))) _68)))))))))))) (letrec ((string->list (lambda (s) (letrec ((f (lambda (i) (let ((_69 (string-length s))) (let ((_70 (< i _69))) (if _70 (let ((_73 (string-ref s i))) (let ((_74 (+ i 1))) (let ((_71 _74)) (let ((_75 (f _71))) (let ((_72 _75)) (let ((_76 (cons _73 _72))) _76)))))) (quote ()))))))) (f 0))))) (letrec ((void (lambda () #f))) (letrec ((tagged-list? (lambda (tag l) (let ((_79 (pair? l))) (let ((_80 (car l))) (let ((_77 _80)) (let ((_81 (eq? tag _77))) (let ((_78 _81)) (let ((_82 (and _79 _78))) _82))))))))) (letrec ((char->natural (lambda (c) (let ((_83 (char->integer c))) (let ((i _83)) (let ((_84 (< i 0))) (if _84 (* -2 i) (let ((_85 (* 2 i))) (let ((_86 (+ _85 1))) _86))))))))) (letrec ((integer->char-list (lambda (n) (let ((_87 (number->string n))) (let ((_88 (string->list _87))) _88))))) (letrec ((const? (lambda (exp) (integer? exp)))) (letrec ((ref? (lambda (exp) (symbol? exp)))) (letrec ((let? (lambda (exp) (let ((_89 (quote let))) (let ((_90 (tagged-list? _89 exp))) _90))))) (letrec ((let->bindings (lambda (exp) (cadr exp)))) (letrec ((let->exp (lambda (exp) (caddr exp)))) (letrec ((letrec1? (lambda (exp) (let ((_98 (quote letrec))) (let ((_91 _98)) (let ((_99 (tagged-list? _91 exp))) (let ((_92 _99)) (let ((_100 (cadr exp))) (let ((_95 _100)) (let ((_93 _95)) (let ((_101 (length _93))) (let ((_96 _101)) (let ((_94 _96)) (let ((_102 (= _94 1))) (let ((_97 _102)) (let ((_103 (and _92 _97))) _103)))))))))))))))) (letrec ((letrec1->binding (lambda (exp) (caadr exp)))) (letrec ((letrec1->exp (lambda (exp) (caddr exp)))) (letrec ((lambda? (lambda (exp) (let ((_104 (quote lambda))) (let ((_105 (tagged-list? _104 exp))) _105))))) (letrec ((lambda->formals (lambda (exp) (cadr exp)))) (letrec ((lambda->exp (lambda (exp) (caddr exp)))) (letrec ((if? (lambda (exp) (let ((_106 (quote if))) (let ((_107 (tagged-list? _106 exp))) _107))))) (letrec ((if->condition (lambda (exp) (cadr exp)))) (letrec ((if->then (lambda (exp) (caddr exp)))) (letrec ((if->else (lambda (exp) (cadddr exp)))) (letrec ((app? (lambda (exp) (pair? exp)))) (letrec ((app->fun (lambda (exp) (car exp)))) (letrec ((app->args (lambda (exp) (cdr exp)))) (letrec ((prim? (lambda (exp) (let ((_118 (quote +))) (let ((_108 _118)) (let ((_119 (eq? exp _108))) (let ((_109 _119)) (let ((_120 (quote -))) (let ((_110 _120)) (let ((_121 (eq? exp _110))) (let ((_111 _121)) (let ((_122 (quote *))) (let ((_112 _122)) (let ((_123 (eq? exp _112))) (let ((_113 _123)) (let ((_124 (quote =))) (let ((_114 _124)) (let ((_125 (eq? exp _114))) (let ((_115 _125)) (let ((_126 (quote display))) (let ((_116 _126)) (let ((_127 (eq? exp _116))) (let ((_117 _127)) (let ((_128 (or _109 _111 _113 _115 _117))) _128)))))))))))))))))))))))) (letrec ((begin? (lambda (exp) (let ((_129 (quote begin))) (let ((_130 (tagged-list? _129 exp))) _130))))) (letrec ((begin->exps (lambda (exp) (cdr exp)))) (letrec ((set!? (lambda (exp) (let ((_131 (quote set!))) (let ((_132 (tagged-list? _131 exp))) _132))))) (letrec ((set!-var (lambda (exp) (cadr exp)))) (letrec ((set!-exp (lambda (exp) (caddr exp)))) (letrec ((let=>lambda (lambda (exp) (let ((_133 (let? exp))) (if _133 (let ((_134 (let->bindings exp))) (let ((_135 (map car _134))) (let ((vars _135)) (let ((_136 (let->bindings exp))) (let ((_137 (map cadr _136))) (let ((args _137)) (let ((_151 (quote lambda))) (let ((_145 _151)) (let ((_152 (let->exp exp))) (let ((_146 _152)) (let ((_141 _146)) (let ((_138 _141)) (let ((_153 (quote ()))) (let ((_147 _153)) (let ((_142 _147)) (let ((_139 _142)) (let ((_154 (cons _138 _139))) (let ((_148 _154)) (let ((_143 _148)) (let ((_140 _143)) (let ((_155 (cons vars _140))) (let ((_149 _155)) (let ((_144 _149)) (let ((_156 (cons _145 _144))) (let ((_150 _156)) (let ((_157 (cons _150 args))) _157)))))))))))))))))))))))))) exp))))) (letrec ((arity (lambda (lam) (let ((_158 (lambda->formals lam))) (let ((_159 (length _158))) _159))))) (letrec ((xargs (lambda (n) (let ((_160 (<= n 0))) (if _160 (quote ()) (let ((_168 (number->string n))) (let ((_163 _168)) (let ((_161 _163)) (let ((_169 (string-append "x" _161))) (let ((_164 _169)) (let ((_162 _164)) (let ((_170 (string->symbol _162))) (let ((_165 _170)) (let ((_171 (- n 1))) (let ((_166 _171)) (let ((_172 (xargs _166))) (let ((_167 _172)) (let ((_173 (cons _165 _167))) _173)))))))))))))))))) (letrec ((Yn (lambda (n) (let ((_821 (quote lambda))) (let ((_441 _821)) (let ((_822 (quote h))) (let ((_442 _822)) (let ((_406 _442)) (let ((_174 _406)) (let ((_823 (quote ()))) (let ((_443 _823)) (let ((_407 _443)) (let ((_175 _407)) (let ((_824 (cons _174 _175))) (let ((_444 _824)) (let ((_408 _444)) (let ((_176 _408)) (let ((_825 (quote lambda))) (let ((_445 _825)) (let ((_409 _445)) (let ((_375 _409)) (let ((_346 _375)) (let ((_826 (quote F))) (let ((_446 _826)) (let ((_410 _446)) (let ((_376 _410)) (let ((_347 _376)) (let ((_319 _347)) (let ((_177 _319)) (let ((_827 (quote ()))) (let ((_447 _827)) (let ((_411 _447)) (let ((_377 _411)) (let ((_348 _377)) (let ((_320 _348)) (let ((_178 _320)) (let ((_828 (cons _177 _178))) (let ((_448 _828)) (let ((_412 _448)) (let ((_378 _412)) (let ((_349 _378)) (let ((_321 _349)) (let ((_179 _321)) (let ((_829 (quote F))) (let ((_449 _829)) (let ((_413 _449)) (let ((_379 _413)) (let ((_350 _379)) (let ((_322 _350)) (let ((_296 _322)) (let ((_275 _296)) (let ((_830 (quote lambda))) (let ((_450 _830)) (let ((_414 _450)) (let ((_380 _414)) (let ((_351 _380)) (let ((_323 _351)) (let ((_297 _323)) (let ((_276 _297)) (let ((_256 _276)) (let ((_239 _256)) (let ((_831 (xargs n))) (let ((_451 _831)) (let ((_415 _451)) (let ((_381 _415)) (let ((_352 _381)) (let ((_324 _352)) (let ((_298 _324)) (let ((_277 _298)) (let ((_257 _277)) (let ((_240 _257)) (let ((_224 _240)) (let ((_832 (quote h))) (let ((_452 _832)) (let ((_416 _452)) (let ((_382 _416)) (let ((_353 _382)) (let ((_325 _353)) (let ((_299 _325)) (let ((_278 _299)) (let ((_258 _278)) (let ((_241 _258)) (let ((_225 _241)) (let ((_211 _225)) (let ((_200 _211)) (let ((_191 _200)) (let ((_183 _191)) (let ((_833 (quote h))) (let ((_453 _833)) (let ((_417 _453)) (let ((_383 _417)) (let ((_354 _383)) (let ((_326 _354)) (let ((_300 _326)) (let ((_279 _300)) (let ((_259 _279)) (let ((_242 _259)) (let ((_226 _242)) (let ((_212 _226)) (let ((_201 _212)) (let ((_192 _201)) (let ((_184 _192)) (let ((_180 _184)) (let ((_834 (quote ()))) (let ((_454 _834)) (let ((_418 _454)) (let ((_384 _418)) (let ((_355 _384)) (let ((_327 _355)) (let ((_301 _327)) (let ((_280 _301)) (let ((_260 _280)) (let ((_243 _260)) (let ((_227 _243)) (let ((_213 _227)) (let ((_202 _213)) (let ((_193 _202)) (let ((_185 _193)) (let ((_181 _185)) (let ((_835 (cons _180 _181))) (let ((_455 _835)) (let ((_419 _455)) (let ((_385 _419)) (let ((_356 _385)) (let ((_328 _356)) (let ((_302 _328)) (let ((_281 _302)) (let ((_261 _281)) (let ((_244 _261)) (let ((_228 _244)) (let ((_214 _228)) (let ((_203 _214)) (let ((_194 _203)) (let ((_186 _194)) (let ((_182 _186)) (let ((_836 (cons _183 _182))) (let ((_456 _836)) (let ((_420 _456)) (let ((_386 _420)) (let ((_357 _386)) (let ((_329 _357)) (let ((_303 _329)) (let ((_282 _303)) (let ((_262 _282)) (let ((_245 _262)) (let ((_229 _245)) (let ((_215 _229)) (let ((_204 _215)) (let ((_195 _204)) (let ((_187 _195)) (let ((_837 (quote F))) (let ((_457 _837)) (let ((_421 _457)) (let ((_387 _421)) (let ((_358 _387)) (let ((_330 _358)) (let ((_304 _330)) (let ((_283 _304)) (let ((_263 _283)) (let ((_246 _263)) (let ((_230 _246)) (let ((_216 _230)) (let ((_205 _216)) (let ((_196 _205)) (let ((_188 _196)) (let ((_838 (quote ()))) (let ((_458 _838)) (let ((_422 _458)) (let ((_388 _422)) (let ((_359 _388)) (let ((_331 _359)) (let ((_305 _331)) (let ((_284 _305)) (let ((_264 _284)) (let ((_247 _264)) (let ((_231 _247)) (let ((_217 _231)) (let ((_206 _217)) (let ((_197 _206)) (let ((_189 _197)) (let ((_839 (cons _188 _189))) (let ((_459 _839)) (let ((_423 _459)) (let ((_389 _423)) (let ((_360 _389)) (let ((_332 _360)) (let ((_306 _332)) (let ((_285 _306)) (let ((_265 _285)) (let ((_248 _265)) (let ((_232 _248)) (let ((_218 _232)) (let ((_207 _218)) (let ((_198 _207)) (let ((_190 _198)) (let ((_840 (cons _187 _190))) (let ((_460 _840)) (let ((_424 _460)) (let ((_390 _424)) (let ((_361 _390)) (let ((_333 _361)) (let ((_307 _333)) (let ((_286 _307)) (let ((_266 _286)) (let ((_249 _266)) (let ((_233 _249)) (let ((_219 _233)) (let ((_208 _219)) (let ((_199 _208)) (let ((_841 (xargs n))) (let ((_461 _841)) (let ((_425 _461)) (let ((_391 _425)) (let ((_362 _391)) (let ((_334 _362)) (let ((_308 _334)) (let ((_287 _308)) (let ((_267 _287)) (let ((_250 _267)) (let ((_234 _250)) (let ((_220 _234)) (let ((_209 _220)) (let ((_842 (cons _199 _209))) (let ((_462 _842)) (let ((_426 _462)) (let ((_392 _426)) (let ((_363 _392)) (let ((_335 _363)) (let ((_309 _335)) (let ((_288 _309)) (let ((_268 _288)) (let ((_251 _268)) (let ((_235 _251)) (let ((_221 _235)) (let ((_210 _221)) (let ((_843 (quote ()))) (let ((_463 _843)) (let ((_427 _463)) (let ((_393 _427)) (let ((_364 _393)) (let ((_336 _364)) (let ((_310 _336)) (let ((_289 _310)) (let ((_269 _289)) (let ((_252 _269)) (let ((_236 _252)) (let ((_222 _236)) (let ((_844 (cons _210 _222))) (let ((_464 _844)) (let ((_428 _464)) (let ((_394 _428)) (let ((_365 _394)) (let ((_337 _365)) (let ((_311 _337)) (let ((_290 _311)) (let ((_270 _290)) (let ((_253 _270)) (let ((_237 _253)) (let ((_223 _237)) (let ((_845 (cons _224 _223))) (let ((_465 _845)) (let ((_429 _465)) (let ((_395 _429)) (let ((_366 _395)) (let ((_338 _366)) (let ((_312 _338)) (let ((_291 _312)) (let ((_271 _291)) (let ((_254 _271)) (let ((_238 _254)) (let ((_846 (cons _239 _238))) (let ((_466 _846)) (let ((_430 _466)) (let ((_396 _430)) (let ((_367 _396)) (let ((_339 _367)) (let ((_313 _339)) (let ((_292 _313)) (let ((_272 _292)) (let ((_255 _272)) (let ((_847 (quote ()))) (let ((_467 _847)) (let ((_431 _467)) (let ((_397 _431)) (let ((_368 _397)) (let ((_340 _368)) (let ((_314 _340)) (let ((_293 _314)) (let ((_273 _293)) (let ((_848 (cons _255 _273))) (let ((_468 _848)) (let ((_432 _468)) (let ((_398 _432)) (let ((_369 _398)) (let ((_341 _369)) (let ((_315 _341)) (let ((_294 _315)) (let ((_274 _294)) (let ((_849 (cons _275 _274))) (let ((_469 _849)) (let ((_433 _469)) (let ((_399 _433)) (let ((_370 _399)) (let ((_342 _370)) (let ((_316 _342)) (let ((_295 _316)) (let ((_850 (quote ()))) (let ((_470 _850)) (let ((_434 _470)) (let ((_400 _434)) (let ((_371 _400)) (let ((_343 _371)) (let ((_317 _343)) (let ((_851 (cons _295 _317))) (let ((_471 _851)) (let ((_435 _471)) (let ((_401 _435)) (let ((_372 _401)) (let ((_344 _372)) (let ((_318 _344)) (let ((_852 (cons _179 _318))) (let ((_472 _852)) (let ((_436 _472)) (let ((_402 _436)) (let ((_373 _402)) (let ((_345 _373)) (let ((_853 (cons _346 _345))) (let ((_473 _853)) (let ((_437 _473)) (let ((_403 _437)) (let ((_374 _403)) (let ((_854 (quote ()))) (let ((_474 _854)) (let ((_438 _474)) (let ((_404 _438)) (let ((_855 (cons _374 _404))) (let ((_475 _855)) (let ((_439 _475)) (let ((_405 _439)) (let ((_856 (cons _176 _405))) (let ((_476 _856)) (let ((_440 _476)) (let ((_857 (cons _441 _440))) (let ((_477 _857)) (let ((_858 (quote lambda))) (let ((_782 _858)) (let ((_745 _782)) (let ((_859 (quote h))) (let ((_783 _859)) (let ((_746 _783)) (let ((_710 _746)) (let ((_478 _710)) (let ((_860 (quote ()))) (let ((_784 _860)) (let ((_747 _784)) (let ((_711 _747)) (let ((_479 _711)) (let ((_861 (cons _478 _479))) (let ((_785 _861)) (let ((_748 _785)) (let ((_712 _748)) (let ((_480 _712)) (let ((_862 (quote lambda))) (let ((_786 _862)) (let ((_749 _786)) (let ((_713 _749)) (let ((_679 _713)) (let ((_650 _679)) (let ((_863 (quote F))) (let ((_787 _863)) (let ((_750 _787)) (let ((_714 _750)) (let ((_680 _714)) (let ((_651 _680)) (let ((_623 _651)) (let ((_481 _623)) (let ((_864 (quote ()))) (let ((_788 _864)) (let ((_751 _788)) (let ((_715 _751)) (let ((_681 _715)) (let ((_652 _681)) (let ((_624 _652)) (let ((_482 _624)) (let ((_865 (cons _481 _482))) (let ((_789 _865)) (let ((_752 _789)) (let ((_716 _752)) (let ((_682 _716)) (let ((_653 _682)) (let ((_625 _653)) (let ((_483 _625)) (let ((_866 (quote F))) (let ((_790 _866)) (let ((_753 _790)) (let ((_717 _753)) (let ((_683 _717)) (let ((_654 _683)) (let ((_626 _654)) (let ((_600 _626)) (let ((_579 _600)) (let ((_867 (quote lambda))) (let ((_791 _867)) (let ((_754 _791)) (let ((_718 _754)) (let ((_684 _718)) (let ((_655 _684)) (let ((_627 _655)) (let ((_601 _627)) (let ((_580 _601)) (let ((_560 _580)) (let ((_543 _560)) (let ((_868 (xargs n))) (let ((_792 _868)) (let ((_755 _792)) (let ((_719 _755)) (let ((_685 _719)) (let ((_656 _685)) (let ((_628 _656)) (let ((_602 _628)) (let ((_581 _602)) (let ((_561 _581)) (let ((_544 _561)) (let ((_528 _544)) (let ((_869 (quote h))) (let ((_793 _869)) (let ((_756 _793)) (let ((_720 _756)) (let ((_686 _720)) (let ((_657 _686)) (let ((_629 _657)) (let ((_603 _629)) (let ((_582 _603)) (let ((_562 _582)) (let ((_545 _562)) (let ((_529 _545)) (let ((_515 _529)) (let ((_504 _515)) (let ((_495 _504)) (let ((_487 _495)) (let ((_870 (quote h))) (let ((_794 _870)) (let ((_757 _794)) (let ((_721 _757)) (let ((_687 _721)) (let ((_658 _687)) (let ((_630 _658)) (let ((_604 _630)) (let ((_583 _604)) (let ((_563 _583)) (let ((_546 _563)) (let ((_530 _546)) (let ((_516 _530)) (let ((_505 _516)) (let ((_496 _505)) (let ((_488 _496)) (let ((_484 _488)) (let ((_871 (quote ()))) (let ((_795 _871)) (let ((_758 _795)) (let ((_722 _758)) (let ((_688 _722)) (let ((_659 _688)) (let ((_631 _659)) (let ((_605 _631)) (let ((_584 _605)) (let ((_564 _584)) (let ((_547 _564)) (let ((_531 _547)) (let ((_517 _531)) (let ((_506 _517)) (let ((_497 _506)) (let ((_489 _497)) (let ((_485 _489)) (let ((_872 (cons _484 _485))) (let ((_796 _872)) (let ((_759 _796)) (let ((_723 _759)) (let ((_689 _723)) (let ((_660 _689)) (let ((_632 _660)) (let ((_606 _632)) (let ((_585 _606)) (let ((_565 _585)) (let ((_548 _565)) (let ((_532 _548)) (let ((_518 _532)) (let ((_507 _518)) (let ((_498 _507)) (let ((_490 _498)) (let ((_486 _490)) (let ((_873 (cons _487 _486))) (let ((_797 _873)) (let ((_760 _797)) (let ((_724 _760)) (let ((_690 _724)) (let ((_661 _690)) (let ((_633 _661)) (let ((_607 _633)) (let ((_586 _607)) (let ((_566 _586)) (let ((_549 _566)) (let ((_533 _549)) (let ((_519 _533)) (let ((_508 _519)) (let ((_499 _508)) (let ((_491 _499)) (let ((_874 (quote F))) (let ((_798 _874)) (let ((_761 _798)) (let ((_725 _761)) (let ((_691 _725)) (let ((_662 _691)) (let ((_634 _662)) (let ((_608 _634)) (let ((_587 _608)) (let ((_567 _587)) (let ((_550 _567)) (let ((_534 _550)) (let ((_520 _534)) (let ((_509 _520)) (let ((_500 _509)) (let ((_492 _500)) (let ((_875 (quote ()))) (let ((_799 _875)) (let ((_762 _799)) (let ((_726 _762)) (let ((_692 _726)) (let ((_663 _692)) (let ((_635 _663)) (let ((_609 _635)) (let ((_588 _609)) (let ((_568 _588)) (let ((_551 _568)) (let ((_535 _551)) (let ((_521 _535)) (let ((_510 _521)) (let ((_501 _510)) (let ((_493 _501)) (let ((_876 (cons _492 _493))) (let ((_800 _876)) (let ((_763 _800)) (let ((_727 _763)) (let ((_693 _727)) (let ((_664 _693)) (let ((_636 _664)) (let ((_610 _636)) (let ((_589 _610)) (let ((_569 _589)) (let ((_552 _569)) (let ((_536 _552)) (let ((_522 _536)) (let ((_511 _522)) (let ((_502 _511)) (let ((_494 _502)) (let ((_877 (cons _491 _494))) (let ((_801 _877)) (let ((_764 _801)) (let ((_728 _764)) (let ((_694 _728)) (let ((_665 _694)) (let ((_637 _665)) (let ((_611 _637)) (let ((_590 _611)) (let ((_570 _590)) (let ((_553 _570)) (let ((_537 _553)) (let ((_523 _537)) (let ((_512 _523)) (let ((_503 _512)) (let ((_878 (xargs n))) (let ((_802 _878)) (let ((_765 _802)) (let ((_729 _765)) (let ((_695 _729)) (let ((_666 _695)) (let ((_638 _666)) (let ((_612 _638)) (let ((_591 _612)) (let ((_571 _591)) (let ((_554 _571)) (let ((_538 _554)) (let ((_524 _538)) (let ((_513 _524)) (let ((_879 (cons _503 _513))) (let ((_803 _879)) (let ((_766 _803)) (let ((_730 _766)) (let ((_696 _730)) (let ((_667 _696)) (let ((_639 _667)) (let ((_613 _639)) (let ((_592 _613)) (let ((_572 _592)) (let ((_555 _572)) (let ((_539 _555)) (let ((_525 _539)) (let ((_514 _525)) (let ((_880 (quote ()))) (let ((_804 _880)) (let ((_767 _804)) (let ((_731 _767)) (let ((_697 _731)) (let ((_668 _697)) (let ((_640 _668)) (let ((_614 _640)) (let ((_593 _614)) (let ((_573 _593)) (let ((_556 _573)) (let ((_540 _556)) (let ((_526 _540)) (let ((_881 (cons _514 _526))) (let ((_805 _881)) (let ((_768 _805)) (let ((_732 _768)) (let ((_698 _732)) (let ((_669 _698)) (let ((_641 _669)) (let ((_615 _641)) (let ((_594 _615)) (let ((_574 _594)) (let ((_557 _574)) (let ((_541 _557)) (let ((_527 _541)) (let ((_882 (cons _528 _527))) (let ((_806 _882)) (let ((_769 _806)) (let ((_733 _769)) (let ((_699 _733)) (let ((_670 _699)) (let ((_642 _670)) (let ((_616 _642)) (let ((_595 _616)) (let ((_575 _595)) (let ((_558 _575)) (let ((_542 _558)) (let ((_883 (cons _543 _542))) (let ((_807 _883)) (let ((_770 _807)) (let ((_734 _770)) (let ((_700 _734)) (let ((_671 _700)) (let ((_643 _671)) (let ((_617 _643)) (let ((_596 _617)) (let ((_576 _596)) (let ((_559 _576)) (let ((_884 (quote ()))) (let ((_808 _884)) (let ((_771 _808)) (let ((_735 _771)) (let ((_701 _735)) (let ((_672 _701)) (let ((_644 _672)) (let ((_618 _644)) (let ((_597 _618)) (let ((_577 _597)) (let ((_885 (cons _559 _577))) (let ((_809 _885)) (let ((_772 _809)) (let ((_736 _772)) (let ((_702 _736)) (let ((_673 _702)) (let ((_645 _673)) (let ((_619 _645)) (let ((_598 _619)) (let ((_578 _598)) (let ((_886 (cons _579 _578))) (let ((_810 _886)) (let ((_773 _810)) (let ((_737 _773)) (let ((_703 _737)) (let ((_674 _703)) (let ((_646 _674)) (let ((_620 _646)) (let ((_599 _620)) (let ((_887 (quote ()))) (let ((_811 _887)) (let ((_774 _811)) (let ((_738 _774)) (let ((_704 _738)) (let ((_675 _704)) (let ((_647 _675)) (let ((_621 _647)) (let ((_888 (cons _599 _621))) (let ((_812 _888)) (let ((_775 _812)) (let ((_739 _775)) (let ((_705 _739)) (let ((_676 _705)) (let ((_648 _676)) (let ((_622 _648)) (let ((_889 (cons _483 _622))) (let ((_813 _889)) (let ((_776 _813)) (let ((_740 _776)) (let ((_706 _740)) (let ((_677 _706)) (let ((_649 _677)) (let ((_890 (cons _650 _649))) (let ((_814 _890)) (let ((_777 _814)) (let ((_741 _777)) (let ((_707 _741)) (let ((_678 _707)) (let ((_891 (quote ()))) (let ((_815 _891)) (let ((_778 _815)) (let ((_742 _778)) (let ((_708 _742)) (let ((_892 (cons _678 _708))) (let ((_816 _892)) (let ((_779 _816)) (let ((_743 _779)) (let ((_709 _743)) (let ((_893 (cons _480 _709))) (let ((_817 _893)) (let ((_780 _817)) (let ((_744 _780)) (let ((_894 (cons _745 _744))) (let ((_818 _894)) (let ((_781 _818)) (let ((_895 (quote ()))) (let ((_819 _895)) (let ((_896 (cons _781 _819))) (let ((_820 _896)) (let ((_897 (cons _477 _820))) _897))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (letrec ((letrec1=>Y (lambda (exp) (let ((_898 (letrec1? exp))) (if _898 (let ((_899 (letrec1->binding exp))) (let ((binding _899)) (let ((_900 (car binding))) (let ((name _900)) (let ((_901 (cadr binding))) (let ((arg _901)) (let ((_902 (arity arg))) (let ((num-args _902)) (let ((_1005 (quote let))) (let ((_1006 (Yn num-args))) (let ((_985 _1006)) (let ((_966 _985)) (let ((_952 _966)) (let ((_939 _952)) (let ((_928 _939)) (let ((_1007 (quote lambda))) (let ((_986 _1007)) (let ((_967 _986)) (let ((_953 _967)) (let ((_940 _953)) (let ((_929 _940)) (let ((_919 _929)) (let ((_912 _919)) (let ((_1008 (quote ()))) (let ((_987 _1008)) (let ((_968 _987)) (let ((_954 _968)) (let ((_941 _954)) (let ((_930 _941)) (let ((_920 _930)) (let ((_913 _920)) (let ((_907 _913)) (let ((_903 _907)) (let ((_1009 (cons name _903))) (let ((_988 _1009)) (let ((_969 _988)) (let ((_955 _969)) (let ((_942 _955)) (let ((_931 _942)) (let ((_921 _931)) (let ((_914 _921)) (let ((_908 _914)) (let ((_904 _908)) (let ((_1010 (quote ()))) (let ((_989 _1010)) (let ((_970 _989)) (let ((_956 _970)) (let ((_943 _956)) (let ((_932 _943)) (let ((_922 _932)) (let ((_915 _922)) (let ((_909 _915)) (let ((_905 _909)) (let ((_1011 (cons arg _905))) (let ((_990 _1011)) (let ((_971 _990)) (let ((_957 _971)) (let ((_944 _957)) (let ((_933 _944)) (let ((_923 _933)) (let ((_916 _923)) (let ((_910 _916)) (let ((_906 _910)) (let ((_1012 (cons _904 _906))) (let ((_991 _1012)) (let ((_972 _991)) (let ((_958 _972)) (let ((_945 _958)) (let ((_934 _945)) (let ((_924 _934)) (let ((_917 _924)) (let ((_911 _917)) (let ((_1013 (cons _912 _911))) (let ((_992 _1013)) (let ((_973 _992)) (let ((_959 _973)) (let ((_946 _959)) (let ((_935 _946)) (let ((_925 _935)) (let ((_918 _925)) (let ((_1014 (quote ()))) (let ((_993 _1014)) (let ((_974 _993)) (let ((_960 _974)) (let ((_947 _960)) (let ((_936 _947)) (let ((_926 _936)) (let ((_1015 (cons _918 _926))) (let ((_994 _1015)) (let ((_975 _994)) (let ((_961 _975)) (let ((_948 _961)) (let ((_937 _948)) (let ((_927 _937)) (let ((_1016 (cons _928 _927))) (let ((_995 _1016)) (let ((_976 _995)) (let ((_962 _976)) (let ((_949 _962)) (let ((_938 _949)) (let ((_1017 (quote ()))) (let ((_996 _1017)) (let ((_977 _996)) (let ((_963 _977)) (let ((_950 _963)) (let ((_1018 (cons _938 _950))) (let ((_997 _1018)) (let ((_978 _997)) (let ((_964 _978)) (let ((_951 _964)) (let ((_1019 (cons name _951))) (let ((_998 _1019)) (let ((_979 _998)) (let ((_965 _979)) (let ((_1020 (quote ()))) (let ((_999 _1020)) (let ((_980 _999)) (let ((_1021 (cons _965 _980))) (let ((_1000 _1021)) (let ((_981 _1000)) (let ((_1022 (letrec1-exp exp))) (let ((_1001 _1022)) (let ((_982 _1001)) (let ((_1023 (quote ()))) (let ((_1002 _1023)) (let ((_983 _1002)) (let ((_1024 (cons _982 _983))) (let ((_1003 _1024)) (let ((_984 _1003)) (let ((_1025 (cons _981 _984))) (let ((_1004 _1025)) (let ((_1026 (cons _1005 _1004))) _1026)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) exp))))) (letrec ((singlet? (lambda (l) (let ((_1029 (list? l))) (let ((_1030 (length l))) (let ((_1027 _1030)) (let ((_1031 (= _1027 1))) (let ((_1028 _1031)) (let ((_1032 (and _1029 _1028))) _1032))))))))) (letrec ((dummy-bind (lambda (exps) (let ((_1033 (singlet? exps))) (if _1033 (car exps) (let ((_1034 (pair? exps))) (if _1034 (let ((_1068 (quote let))) (let ((_1069 (quote $_))) (let ((_1056 _1069)) (let ((_1043 _1056)) (let ((_1038 _1043)) (let ((_1070 (car exps))) (let ((_1057 _1070)) (let ((_1044 _1057)) (let ((_1039 _1044)) (let ((_1035 _1039)) (let ((_1071 (quote ()))) (let ((_1058 _1071)) (let ((_1045 _1058)) (let ((_1040 _1045)) (let ((_1036 _1040)) (let ((_1072 (cons _1035 _1036))) (let ((_1059 _1072)) (let ((_1046 _1059)) (let ((_1041 _1046)) (let ((_1037 _1041)) (let ((_1073 (cons _1038 _1037))) (let ((_1060 _1073)) (let ((_1047 _1060)) (let ((_1042 _1047)) (let ((_1074 (quote ()))) (let ((_1061 _1074)) (let ((_1048 _1061)) (let ((_1075 (cons _1042 _1048))) (let ((_1062 _1075)) (let ((_1049 _1062)) (let ((_1076 (cdr exps))) (let ((_1063 _1076)) (let ((_1052 _1063)) (let ((_1050 _1052)) (let ((_1077 (dummy-bind _1050))) (let ((_1064 _1077)) (let ((_1053 _1064)) (let ((_1051 _1053)) (let ((_1078 (quote ()))) (let ((_1065 _1078)) (let ((_1054 _1065)) (let ((_1079 (cons _1051 _1054))) (let ((_1066 _1079)) (let ((_1055 _1066)) (let ((_1080 (cons _1049 _1055))) (let ((_1067 _1080)) (let ((_1081 (cons _1068 _1067))) _1081))))))))))))))))))))))))))))))))))))))))))))))) (error "no match")))))))) (letrec ((begin=>let (lambda (exp) (let ((_1082 (begin->exps exp))) (let ((_1083 (dummy-bind _1082))) _1083))))) (let ((_1084 (quote ()))) (let ((mutable-variables _1084)) (letrec ((mark-mutable (lambda (symbol) (let ((_1085 (cons symbol mutable-variables))) (set! mutable-variables _1085))))) (letrec ((is-in? (lambda (S symbol) (let ((_1086 (pair? S))) (let ((_1087 (not _1086))) (if _1087 #f (let ((_1088 (car S))) (let ((_1089 (eq? _1088 symbol))) (if _1089 #t (let ((_1090 (cdr S))) (let ((_1091 (is-in? _1090 symbol))) _1091))))))))))) (letrec ((is-mutable? (lambda (symbol) (is-in? mutable-variables symbol)))) (letrec ((analyze-mutable-variables (lambda (exp) (let ((_1092 (const? exp))) (if _1092 (void) (let ((_1093 (ref? exp))) (if _1093 (void) (let ((_1094 (prim? exp))) (if _1094 (void) (let ((_1095 (lambda? exp))) (if _1095 (let ((_1096 (lambda->exp exp))) (let ((_1097 (analyze-mutable-variables _1096))) _1097)) (let ((_1098 (let? exp))) (if _1098 (let ((_1105 (let->bindings exp))) (let ((_1102 _1105)) (let ((_1100 _1102)) (let ((_1106 (map cadr _1100))) (let ((_1103 _1106)) (let ((_1101 _1103)) (let ((_1107 (map analyze-mutable-variables _1101))) (let ((_1104 _1107)) (let ((_1099 _1104)) (let ((_1108 (let->exp exp))) (let ((_1109 (analyze-mutable-variables _1108))) _1109))))))))))) (let ((_1110 (letrec1? exp))) (if _1110 (let ((_1117 (letrec1->binding exp))) (let ((_1114 _1117)) (let ((_1112 _1114)) (let ((_1118 (cadr _1112))) (let ((_1115 _1118)) (let ((_1113 _1115)) (let ((_1119 (analyze-mutable-variables _1113))) (let ((_1116 _1119)) (let ((_1111 _1116)) (let ((_1120 (letrec1->exp exp))) (let ((_1121 (analyze-mutable-variables _1120))) _1121))))))))))) (let ((_1122 (set!? exp))) (if _1122 (let ((_1123 (set!-var exp))) (let ((_1124 (mark-mutable _1123))) _1124)) (let ((_1125 (if? exp))) (if _1125 (let ((_1132 (if->condition exp))) (let ((_1127 _1132)) (let ((_1133 (analyze-mutable-variables _1127))) (let ((_1128 _1133)) (let ((_1126 _1128)) (let ((_1134 (if->then exp))) (let ((_1130 _1134)) (let ((_1135 (analyze-mutable-variables _1130))) (let ((_1131 _1135)) (let ((_1129 _1131)) (let ((_1136 (if->else exp))) (let ((_1137 (analyze-mutable-variables _1136))) _1137)))))))))))) (let ((_1138 (begin? exp))) (if _1138 (let ((_1142 (begin->exps exp))) (let ((_1140 _1142)) (let ((_1143 (map analyze-mutable-variables _1140))) (let ((_1141 _1143)) (let ((_1139 _1141)) (void)))))) (let ((_1144 (app? exp))) (if _1144 (let ((_1146 (map analyze-mutable-variables exp))) (let ((_1145 _1146)) (void))) (error "unknown expression type: " exp)))))))))))))))))))))))) (letrec ((m (lambda (chars) (let ((_1147 (null? chars))) (if _1147 (quote ()) (let ((_1148 (car chars))) (let ((_1149 (char-alphabetic? _1148))) (let ((_1150 (car chars))) (let ((_1151 (char=? _1150 #\_))) (let ((_1152 (not _1151))) (let ((_1153 (and _1149 _1152))) (let ((_1154 (car chars))) (let ((_1155 (char-numeric? _1154))) (let ((_1156 (or _1153 _1155))) (if _1156 (let ((_1159 (car chars))) (let ((_1160 (cdr chars))) (let ((_1157 _1160)) (let ((_1161 (m _1157))) (let ((_1158 _1161)) (let ((_1162 (cons _1159 _1158))) _1162)))))) (let ((_1176 (car chars))) (let ((_1170 _1176)) (let ((_1165 _1170)) (let ((_1163 _1165)) (let ((_1177 (char->natural _1163))) (let ((_1171 _1177)) (let ((_1166 _1171)) (let ((_1164 _1166)) (let ((_1178 (integer->char-list _1164))) (let ((_1172 _1178)) (let ((_1167 _1172)) (let ((_1179 (cdr chars))) (let ((_1173 _1179)) (let ((_1168 _1173)) (let ((_1180 (m _1168))) (let ((_1174 _1180)) (let ((_1169 _1174)) (let ((_1181 (append _1167 _1169))) (let ((_1175 _1181)) (let ((_1182 (cons #\_ _1175))) _1182))))))))))))))))))))))))))))))))))) (letrec ((mangle (lambda (symbol) (let ((_1188 (symbol->string symbol))) (let ((_1185 _1188)) (let ((_1183 _1185)) (let ((_1189 (string->list _1183))) (let ((_1186 _1189)) (let ((_1184 _1186)) (let ((_1190 (m _1184))) (let ((_1187 _1190)) (let ((_1191 (list->string _1187))) _1191)))))))))))) (letrec ((java-compile-const (lambda (exp) (let ((_1192 (integer? exp))) (if _1192 (let ((_1193 (number->string exp))) (let ((_1194 (string-append "new IntValue(" _1193 ")"))) _1194)) (error "unknown constant: " exp)))))) (letrec ((java-compile-prim (lambda (p) (let ((_1195 (quote +))) (let ((_1196 (eq? _1195 p))) (if _1196 "sum" (let ((_1197 (quote -))) (let ((_1198 (eq? _1197 p))) (if _1198 "difference" (let ((_1199 (quote *))) (let ((_1200 (eq? _1199 p))) (if _1200 "product" (let ((_1201 (quote =))) (let ((_1202 (eq? _1201 p))) (if _1202 "numEqual" (let ((_1203 (quote display))) (let ((_1204 (eq? _1203 p))) (if _1204 "display" (error "unhandled primitive " p))))))))))))))))))) (letrec ((java-compile-ref (lambda (exp) (let ((_1205 (is-mutable? exp))) (if _1205 (let ((_1206 (mangle exp))) (let ((_1207 (string-append "m_" _1206 ".value"))) _1207)) (mangle exp)))))) (letrec ((java-compile-formals (lambda (formals) (let ((_1208 (pair? formals))) (let ((_1209 (not _1208))) (if _1209 "" (let ((_1219 (car formals))) (let ((_1210 _1219)) (let ((_1220 (mangle _1210))) (let ((_1211 _1220)) (let ((_1221 (cdr formals))) (let ((_1212 _1221)) (let ((_1222 (pair? _1212))) (let ((_1213 _1222)) (let ((_1223 (if _1213 (let ((_1216 (cdr formals))) (let ((_1214 _1216)) (let ((_1217 (java-compile-formals _1214))) (let ((_1215 _1217)) (let ((_1218 (string-append ", " _1215))) _1218))))) ""))) (let ((_1224 (string-append "final Value " _1211 _1223))) _1224)))))))))))))))) (letrec ((java-wrap-mutables (lambda (vars) (let ((_1225 (pair? vars))) (let ((_1226 (not _1225))) (if _1226 "" (let ((_1240 (car vars))) (let ((_1227 _1240)) (let ((_1241 (is-mutable? _1227))) (let ((_1228 _1241)) (let ((_1242 (if _1228 (let ((_1233 (car vars))) (let ((_1229 _1233)) (let ((_1234 (mangle _1229))) (let ((_1230 _1234)) (let ((_1235 (car vars))) (let ((_1231 _1235)) (let ((_1236 (mangle _1231))) (let ((_1232 _1236)) (let ((_1237 (string-append " final ValueCell m_" _1230 " = new ValueCell(" _1232 ");\n"))) _1237))))))))) ""))) (let ((_1243 (cdr vars))) (let ((_1238 _1243)) (let ((_1244 (java-wrap-mutables _1238))) (let ((_1239 _1244)) (let ((_1245 (string-append _1242 _1239))) _1245)))))))))))))))) (letrec ((java-compile-lambda (lambda (exp) (let ((_1246 (lambda->formals exp))) (let ((formals _1246)) (let ((_1247 (length formals))) (let ((num-args _1247)) (let ((_1250 (number->string num-args))) (let ((_1251 (java-compile-formals formals))) (let ((_1252 (java-wrap-mutables formals))) (let ((_1253 (lambda->exp exp))) (let ((_1248 _1253)) (let ((_1254 (java-compile-exp _1248))) (let ((_1249 _1254)) (let ((_1255 (string-append "new NullProcValue" _1250 " () {\n" " public Value apply(" _1251 ") {\n" _1252 "\n" "  return " _1249 " ;\n" "}}\n"))) _1255))))))))))))))) (letrec ((java-compile-args (lambda (args) (let ((_1256 (pair? args))) (let ((_1257 (not _1256))) (if _1257 "" (let ((_1267 (car args))) (let ((_1258 _1267)) (let ((_1268 (java-compile-exp _1258))) (let ((_1259 _1268)) (let ((_1269 (cdr args))) (let ((_1260 _1269)) (let ((_1270 (pair? _1260))) (let ((_1261 _1270)) (let ((_1271 (if _1261 (let ((_1264 (cdr args))) (let ((_1262 _1264)) (let ((_1265 (java-compile-args _1262))) (let ((_1263 _1265)) (let ((_1266 (string-append ", " _1263))) _1266))))) ""))) (let ((_1272 (string-append _1259 _1271))) _1272)))))))))))))))) (letrec ((java-compile-set! (lambda (exp) (let ((_1277 (set!-var exp))) (let ((_1273 _1277)) (let ((_1278 (mangle _1273))) (let ((_1274 _1278)) (let ((_1279 (set!-exp exp))) (let ((_1275 _1279)) (let ((_1280 (java-compile-exp _1275))) (let ((_1276 _1280)) (let ((_1281 (string-append "VoidValue.Void(m_" _1274 ".value = " _1276 ")"))) _1281)))))))))))) (letrec ((java-compile-app (lambda (exp) (let ((_1282 (app->args exp))) (let ((args _1282)) (let ((_1283 (app->fun exp))) (let ((fun _1283)) (let ((_1284 (length args))) (let ((num-args _1284)) (let ((_1285 (number->string num-args))) (let ((_1286 (java-compile-exp fun))) (let ((_1287 (java-compile-args args))) (let ((_1288 (string-append "((ProcValue" _1285 ")(" _1286 ")).apply(" _1287 ")\n"))) _1288))))))))))))) (letrec ((java-compile-if (lambda (exp) (let ((_1295 (if->condition exp))) (let ((_1289 _1295)) (let ((_1296 (java-compile-exp _1289))) (let ((_1290 _1296)) (let ((_1297 (if->then exp))) (let ((_1291 _1297)) (let ((_1298 (java-compile-exp _1291))) (let ((_1292 _1298)) (let ((_1299 (if->else exp))) (let ((_1293 _1299)) (let ((_1300 (java-compile-exp _1293))) (let ((_1294 _1300)) (let ((_1301 (string-append "(" _1290 ").toBoolean() ? (" _1292 ") : (" _1294 ")"))) _1301)))))))))))))))) (letrec ((java-compile-exp (lambda (exp) (let ((_1302 (const? exp))) (if _1302 (java-compile-const exp) (let ((_1303 (prim? exp))) (if _1303 (java-compile-prim exp) (let ((_1304 (ref? exp))) (if _1304 (java-compile-ref exp) (let ((_1305 (lambda? exp))) (if _1305 (java-compile-lambda exp) (let ((_1306 (if? exp))) (if _1306 (java-compile-if exp) (let ((_1307 (set!? exp))) (if _1307 (java-compile-set! exp) (let ((_1308 (let? exp))) (if _1308 (let ((_1309 (let=>lambda exp))) (let ((_1310 (java-compile-exp _1309))) _1310)) (let ((_1311 (letrec1? exp))) (if _1311 (let ((_1312 (letrec1=>Y exp))) (let ((_1313 (java-compile-exp _1312))) _1313)) (let ((_1314 (begin? exp))) (if _1314 (let ((_1315 (begin=>let exp))) (let ((_1316 (java-compile-exp _1315))) _1316)) (let ((_1317 (app? exp))) (if _1317 (java-compile-app exp) (error "no match")))))))))))))))))))))))) (letrec ((java-compile-program (lambda (exp) (let ((_1318 (java-compile-exp exp))) (let ((_1319 (string-append "public class BOut extends RuntimeEnvironment {\n" " public static void main (String[] args) {\n" _1318 " ;\n" " }\n" "}\n"))) _1319))))) (let ((input-program 3)) (let ((_1321 (analyze-mutable-variables input-program))) (let ((_1320 _1321)) (java-compile-program input-program)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
