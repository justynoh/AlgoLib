(* extremepoints.ml
 * Implements the extreme points convex hull algorithm.
 *)

open Core

module CHTest : CONVEXHULL -> TESTER = functor (CH : CONVEXHULL) ->
  struct

    exception TestFailed of int

    type input = CH.input
    type output = CH.output

    let testdata = [|

    (* Test 1: Basic triangle. *)
    (
      [(0.,0.);(1.,0.);(0.,1.)],

      [(0.,0.);(1.,0.);(0.,1.)]
    );

    (* Test 2: A square with interior points. *)
    (
      [(0.,0.);(1.,0.);(0.5,0.4);(0.25,0.3);(0.75,0.1);(0.,1.);(1.,1.)],

      [(0.,0.);(1.,0.);(1.,1.);(0.,1.)]
    );

    (* Test 3: The same square, now with a couple more points. *)
    (
      [(1.,0.);(0.24836794671236148,0.83695152839392883);
      (0.96073877267846219,0.49750941306533941) ;
      (0.16808918334983242,0.52277234093759573) ;
      (0.,0.) ;
      (0.15754503706512493,0.070127412436667008) ;
      (0.95946643566609813,0.68766109547025922) ;
      (0.933052883309281,0.77175497114765934) ; (1.,1.) ; (0.,1.)],

      [(0.,0.);(1.,0.);(1.,1.);(0.,1.)]
    );

    (* Test 4: A more rigorous test case with 100 points. *)
    (
      [(-9.,0.) ; (-1.0278843611363824,-7.286190544046967) ; (3.0786102247135965,0.80818850733622227) ;
      (-1.4338467530956427,-2.9371944694549059) ; (-5.3996182076736963,-0.22067045941202856) ;
      (-6.4930732759027556,-3.6520945164471588) ; (-5.942998605773786,-0.18081951453254774) ;
      (-7.2753468551366041,-5.5708015264042778) ; (-5.3684625267477148,6.2883054090674086) ;
      (0.77348126210549317,-7.4121800962922624) ; (-3.3007536981550567,0.91714658773741675) ;
      (1.0318795946012358,-5.8764231608586428) ; (4.9212598766901383,0.10064497153483032) ;
      (3.6011359204817737,-6.5537243635667046) ; (-1.6107480239045833,-5.5858081897188017) ;
      (4.1531845684078945,-2.9506038218919208) ; (-2.5198374642713635,2.6356775046791743) ;
      (-1.9244548112866928,1.3424816623176632) ; (-7.5063604373054735,-5.0763538987933892) ;
      (1.6964977027145203,-5.8964866758311265) ; (1.0207675415543314,-5.7650671949538657) ;
      (3.477305004694113,-6.4848126432852773) ; (7.,7.) ; (0.784109511685406,-7.5373486969968733) ;
      (4.6074245840114525,6.4416516887099977) ; (-5.5788229539326224,-1.8091916144295341) ;
      (-2.797455475455191,-1.3842144558137406) ; (6.3226473331191446,-1.998244817391611) ;
      (-8.6292879871567472,-1.0735749174558542) ; (3.7779482958447037,-4.77154259151836) ;
      (-2.73752133080219,7.7235137210879437) ; (1.9283882740420779,-4.1571603532933228) ;
      (0.12603040511112695,-8.1441334793563129) ; (-3.0126230238071248,1.0305287754406063) ;
      (5.3232727070748265,4.4752536672417662) ;
      (1.8612722564451918,7.578994700975727) ; (1.5075757466981035,4.1910771717870716) ;
      (1.6503396816563018,3.5239661662976474) ; (-0.81336877896334059,-4.2300459000555808) ;
      (1.8320474563487323,-2.5805485060245381) ; (-2.3039521982694984,-2.7572550011485415) ;
      (1.9732437894649966,1.7755209897415973) ; (3.7915210966474451,-5.2223762656629491) ; (0.,-9.) ;
      (-1.7133204680814789,3.2359759499604568) ; (6.8065487898724513,-1.7204802828462666) ;
      (0.055899405849851647,0.66937323213849709) ; (0.92516213116422286,2.5048547757081021) ;
      (-2.474231422583248,-2.8219331435208233) ; (-6.818593650161759,-6.1495075306520324) ;
      (4.8815886014844647,3.7377161904511755) ; (7.,-7.) ; (-4.4405919971907464,6.7148751736738426) ;
      (-0.13627659997879249,-6.9633867167127192) ; (6.5973759898280608,1.8831680081032172) ;
      (-0.4872469008559257,8.8067813136249) ; (-7.2208843791199318,-0.087878162152179584) ;
      (8.238737668943628,0.461708213965748) ; (5.0660078513114026,-5.6866284654321761) ;
      (-3.7261998169938666,-1.3950579599147819) ; (1.8264707055466989,7.3542520298229448) ; (-7.,-7.) ;
      (2.1960721988292278,2.2926004610541337) ; (-6.9052762069084359,5.8113971277723167) ;
      (-2.7236058681710444,4.7516735718029643) ; (-6.9220658998844886,1.2134924189672187) ;
      (4.8874768942179045,1.8880771513494281) ; (6.9696862433795932,2.2797531097996071) ;
      (5.216524100840056,-5.5643856953917687) ; (-3.4453826065576969,5.8980059311546107) ;
      (6.9950066346285809,3.0787944627884425) ; (-6.31767696915315,-5.8358245981983874) ; (9.,0.) ;
      (1.5329999727318278,-2.2995396400437427) ; (5.844046750267978,-6.9243411142842923) ;
      (-4.6005144764115045,0.053134752500819005) ; (0.69813823177550383,1.7470700507683148) ;
      (-0.24998751372343087,0.46879980862842885) ; (-3.841210824785974,3.8230121564260671) ;
      (-6.5122560175007518,4.0923354169351533) ; (-1.7467324415445677,5.144888498902155) ;
      (-8.123151480066948,-2.7934687104372875) ; (4.5174720962489854,2.9304861390627934) ;
      (-2.6369307454595852,-3.3635277235480565) ; (-3.143087764551959,-1.7702232522846018) ; (-7.,7.) ;
      (8.343536245328238,1.9162110119424494) ; (7.4876745583650575,1.8708285790413441) ;
      (6.6787858692706976,-4.7912765731008156) ; (-5.0811593291546835,4.6631710438747405) ;
      (-4.6975094092273428,-2.4601541490668728) ; (-1.9172245628375331,7.691437062850774) ;
      (-6.6258298467812731,-5.9263260428691922) ; (0.,9.) ; (1.6116501957889753,-0.94437977424631825) ;
      (-4.5314314104754176,-3.6746066748454727) ; (2.9772685286957863,-7.9245623236440013) ;
      (2.2083005647821849,-6.6131167049264405) ; (-3.101765569805095,0.48902269209541238) ;
      (0.69494851536776459,0.66905400423349981) ],

      [(0.,-9.);(7.,-7.);(9.,0.);(7.,7.);(0.,9.);(-7.,7.);(-9.,0.);(-7.,-7.)]
    );

    (* Test 4: A sharp, teardrop-shape hull with an input of size 250. *)
    (
      [ (-9.1288273062376835,0.39140966292470036) ;
        (-4.6626147447740882,0.75407604717917387) ;
        (-1.1751996448386013,-2.303145860028132) ;
        (1.9648029765301196,3.0780634131027487) ;
        (0.4736985717014548,1.4567616104321583) ;
        (-8.68873603181495,0.32730440370443148) ;
        (-4.8476699124998142,2.2587359339548225) ;
        (-5.9968956255007928,1.2697565265295019) ;
        (-9.6339920463878332,-0.96442057391320057) ;
        (-5.4703470249910211,-0.8148372298173352) ;
        (-8.4954906152463181,-0.097992667668594269) ;
        (-4.4232554071296173,-0.81004412447665031) ;
        (-2.9537903973290458,0.92919537452775725) ;
        (-5.5294130084124395,1.7524001718583913) ;
        (-6.5873262021290859,-2.5069617696593869) ;
        (-6.2342450066115687,2.0616416227124841) ;
        (-2.2883677004796104,2.8641439958188881) ;
        (1.041629115429334,2.0085299625327204) ;
        (3.,-0.5) ;
        (3.5231319605252693,3.3050051993261294) ;
        (-4.8349434114210785,-0.60194558180250057) ;
        (4.8620171219235768,2.468037990128841) ;
        (-0.0230785441648802,1.49980972327492) ;
        (-8.6498534861524341,-0.53804870979360153) ;
        (-6.5773287270642991,-2.2390610224405658) ;
        (-3.1049242772467522,-1.0698476980046676) ;
        (-8.5497155443370065,-2.99423175205619) ;
        (2.6802796299683891,2.7657879828991865) ;
        (-6.,-4.) ;
        (-3.7701031100297966,2.1230817344020592) ;
        (3.0126264269917,1.7491815919653106) ;
        (-8.39393198527155,0.16234575515151839) ;
        (-7.4371634099249491,-1.4278003726770705) ;
        (-0.15319127773354246,-2.0722958420338977) ;
        (2.6787836316641478,3.7797685324816195) ;
        (-4.5029743685669477,1.4342857167604812) ;
        (-8.6599590588259439,-0.729261974510973) ;
        (-2.220993840830614,-1.2434994861138193) ;
        (3.5242192173097457,1.9848142906331026) ;
        (0.77008529766750833,1.1867983168655911) ;
        (4.2882499177378364,3.9947886811085365) ;
        (0.87361286062284549,-0.13842869751568365) ;
        (-5.4674555037907924,1.834127186954321) ;
        (-8.4918285639176432,-3.21183345588288) ;
        (1.1830589772580584,-0.86301570367330127) ;
        (1.1762932737785121,2.1800367840040051) ;
        (3.8967181373417148,0.83965579413854918) ;
        (4.9200461105472009,2.0766045471644237) ;
        (-8.,-3.5) ;
        (-3.0020894468360639,-3.4702836193654627) ;
        (-4.3048508537722858,-2.8248482925687188) ;
        (3.4607355478964976,3.2588627934560943) ;
        (-4.3530772864038285,2.729000621724631) ;
        (-6.1569175690187006,-3.5972647969470724) ;
        (-9.1683348285859783,0.57977070745609) ;
        (2.9292486878437547,2.897238200898455) ;
        (-7.8212405767921105,-1.4784677988880319) ;
        (-3.3227348762073508,3.5717045747829239) ;
        (-7.1067125660053208,-3.2047942069088324) ;
        (0.86387924744733624,3.1460710617513019) ;
        (0.0084862027276315644,0.089477953460666448) ;
        (-0.62191445947335211,0.60791249786856572) ;
        (-8.5193823177415613,-1.8091258869148237) ;
        (-5.672669771379856,-0.46299433227824283) ;
        (2.1722797961733118,0.16946937426271713) ;
        (2.0248279458713707,-0.42718345010634717) ;
        (-2.0680845068099778,-3.0200065848182645) ;
        (-3.1212295834995025,2.6134702745502327) ;
        (-2.60589022712926,1.1698065949721288) ;
        (3.,4.) ;
        (-0.09698290382177,2.8047859922278935) ;
        (3.5850515593836469,3.8843816632891981) ;
        (1.1973596758910752,0.527635309224161) ;
        (-8.2133603055886333,-0.71404609582280631) ;
        (-2.,-3.5) ;
        (0.89301492397732218,-0.13677232567222708) ;
        (2.7421965513893145,1.621033914722581) ;
        (0.81942990213864064,0.27607194207200969) ;
        (-5.4827465820829548,-2.1085080797706213) ;
        (-5.0881554885645022,1.2202161615619289) ;
        (-2.0577731522413494,-0.039198281513758815) ;
        (4.7369932306103237,1.8196371671229654) ;
        (-3.9301481705622283,0.69454469112444706) ;
        (-6.5410451829312111,1.6949230027258935) ;
        (-0.45917096097568866,-0.69416205263051811) ;
        (0.80817086894915668,1.9926229986869881) ;
        (-7.7260966342940485,0.54706146508015241) ;
        (-4.5966352950716836,1.7765318860432773) ;
        (-7.9861367427568908,0.18139151417407096) ;
        (-8.0826281301232648,-3.0652744242558816) ;
        (-7.3306122647597896,-1.4476719388548416) ;
        (7.,3.) ;
        (-8.,2.) ;
        (3.5421072964902027,3.60906077662462) ;
        (3.878052609039683,1.69334917291022) ;
        (-4.5574085291153485,-1.6707920418842224) ;
        (4.2518676448442214,1.582811987604658) ;
        (0.4858292111327458,1.7795343727305637) ;
        (3.2538288171892358,1.0926198066299611) ;
        (-2.828191559165484,1.1888860709641413) ;
        (0.54614745164042944,2.1052502241136066) ;
        (5.9101135813215233,2.5873533373349815) ;
        (-1.257536991326214,-2.9502318586344396) ;
        (3.7576121654035042,2.5745166890545592) ;
        (4.8350392432630844,2.2581374397237832) ;
        (-6.660801390331744,1.2465656581773059) ;
        (4.5990785791266688,2.9319631046548764) ;
        (-4.5691159422364214,1.9975975091958471) ;
        (-6.0886756120690109,1.4866735234255071) ;
        (3.1865411291534382,2.9415701441900426) ;
        (3.0865969444963426,0.93191781710350252) ;
        (-6.4660243725111055,-2.90744480739591) ;
        (-3.6225943464503461,1.5873567712712928) ;
        (-6.0664148676426244,2.4886049649326116) ;
        (-9.126482365826563,0.24803371307887279) ;
        (3.2476270881811704,0.33395814124262913) ;
        (-8.0345955098506767,1.8537114587454235) ;
        (2.5920245948768645,0.12376931595504281) ;
        (2.7080274549539585,-0.18194443986244302) ;
        (-1.2911278929328969,0.33033488397742872) ;
        (-2.3783581039466117,0.39628863874254261) ;
        (-2.4515526262107752,0.93613359264546325) ;
        (-10.,0.) ;
        (-2.6993698926107825,-3.421751398823734) ;
        (-7.81120820869413,0.39158393648372058) ;
        (1.,-2.) ;
        (-7.2913789491697134,1.4889502291394665) ;
        (0.98218965288281623,-1.3880230317355835) ;
        (-0.32536274033849288,-2.3409194406264309) ;
        (-4.3626514978332223,2.2043474420486069) ;
        (-10.,-1.) ;
        (-4.3957817327546858,3.2313296747589222) ;
        (3.1430735619787882,0.56819728117723756) ;
        (1.9219691153413905,3.6144717244240656) ;
        (6.3507433101896815,2.7617974880553007) ;
        (-2.2992248260729458,-2.9429327892092312) ;
        (1.5352154338046358,0.85047357197387008) ;
        (0.44408512582698911,3.5766620898122765) ;
        (-8.7937334123297983,0.52574161518919915) ;
        (2.6156372330553754,2.5217058296115109) ;
        (-8.40518180283183,-2.0558765284688532) ;
        (-6.1553161860188812,2.4454487411102752) ;
        (-9.5394760503058027,-1.2100646205326577) ;
        (-3.115698472604052,0.59756356245835462) ;
        (1.3974527774787155,3.6331719256942687) ;
        (-5.7183061302106521,-2.1903918964814135) ;
        (-3.9068037194992087,-0.77512601785832924) ;
        (-7.5637483186032979,-0.90574696623588835) ;
        (-4.4788935318274987,-1.3691170172767881) ;
        (-4.,3.5) ;
        (-2.7805977923242864,2.5173011809482997) ;
        (-6.7493332185592445,0.36732860307523474) ;
        (-0.56931331805580321,3.750670997060003) ;
        (5.1020935794752678,1.4757065011039243) ;
        (-3.3618087512571941,-2.5178585734194234) ;
        (-4.1721923168808139,-2.299952705229078) ;
        (-4.7916528444938207,-1.8791135037071487) ;
        (-7.1247784221818318,-2.3507255974264285) ;
        (-1.9179581168385571,2.5217384210642395) ;
        (-1.30474094268558,-2.2493282903090246) ;
        (-2.6037810257919967,1.4309253123615822) ;
        (-5.40136368480448,-2.9666235980564215) ;
        (-8.9958882851473785,-1.5118059581802092) ;
        (-2.341305173779813,3.2389806289771119) ;
        (-7.7803544516553869,-0.58386617362908133) ;
        (-6.2461744374730976,0.45455179234782683) ;
        (-5.6145579115965374,-1.1379026196099669) ;
        (-0.55311468597837887,-2.4475908192654394) ;
        (-1.9066320993855435,1.026035691015565) ;
        (-2.6934937407267237,-2.2297634002215561) ;
        (-9.78628548370013,-0.42643327806115749) ;
        (-0.40449827476759204,1.4489755763813825) ;
        (-0.93933435139700094,-0.049584035144107474) ;
        (-2.3692439359042821,3.1943968076831935) ;
        (-7.1944282395758563,-3.3632410389411809) ;
        (-8.8514961412776927,-0.665613107531962) ;
        (3.0273958592474823,0.44573376040075008) ;
        (-5.0189640828488784,-0.30576675208773718) ;
        (-4.3390575423625624,2.3171566070083189) ;
        (-3.7726420246846439,-1.160807429165732) ;
        (-2.5582663149135696,1.8788317463478084) ;
        (-2.486942203101222,-2.8205245422622145) ;
        (-3.6299962483346508,0.61714252347331389) ;
        (-5.1581479115039661,-2.6358002747744509) ;
        (-4.8981806631756539,2.3580917294659436) ;
        (0.12663243799755364,-1.5759131275280334) ;
        (-4.5138823012504119,-3.452343468433611) ;
        (-4.4236511523917645,-1.7185656969239771) ;
        (-5.3974743510582357,-1.0376290599806) ;
        (-7.6852467900738866,-2.80074092027647) ;
        (-1.0021689741995186,0.49898525083060008) ;
        (-2.2205505295908328,2.4706469022634767) ;
        (-0.89304704519527434,0.2001493024541201) ;
        (-1.7015409602805232,3.4415786387803449) ;
        (-1.7817226676306515,0.95164127441778046) ;
        (-7.4616943479171027,-0.31781302820111446) ;
        (-9.,-3.) ;
        (-5.8444474607814145,-2.497467660928784) ;
        (5.,4.) ;
        (-2.,3.75) ;
        (-2.0106094132675816,-0.057734451361306505) ;
        (-6.7858684973489591,-0.1256454448175317) ;
        (-9.7678522609007619,-1.2067858521950292) ;
        (-6.2667805330782027,-0.38288881795953644) ;
        (3.726260612014789,1.1279579161058697) ;
        (-1.9148261243382532,-3.0904248809786883) ;
        (-0.31907547619560361,-1.8173353113554152) ;
        (-9.3879040697725138,-0.48486396786292429) ;
        (3.1699328377763489,3.0721460984133246) ;
        (-2.252972415789638,3.076968059043212) ;
        (-2.6925843648465708,-0.85081687625384639) ;
        (-4.4340522055402047,-2.4734120991665964) ;
        (0.27320207185063694,-0.0608583963700573) ;
        (-6.0524296733827772,1.0150058099267847) ;
        (-1.3734549619683509,-0.75787706094369645) ;
        (-7.2167328064643907,-0.78965495596644475) ;
        (-5.21687286380635,-0.8583067102129589) ;
        (-2.8662626886633786,-3.01415954811237) ;
        (1.4387194859292443,0.79043365896592466) ;
        (-6.01324403793817,-0.79117722350494191) ;
        (-0.23918568359455605,3.229280699214871) ;
        (-2.6283172870575306,-1.6526117469097246) ;
        (-4.06949029406608,0.30085617648244956) ;
        (1.8944757112043487,-0.18847134597629811) ;
        (1.9320741386157056,2.5995565926380451) ;
        (-6.4569967417737129,-3.6155980967063339) ;
        (-4.0252493885538412,-1.9229011101447417) ;
        (-1.8787315900816655,0.040617402195182173) ;
        (1.3625673769017546,-0.97099751776173271) ;
        (-1.1952944853230871,-1.0879165973766773) ;
        (5.6327026734108827,2.079696291059296) ;
        (-8.9256974149012827,-1.2645154147782196) ;
        (-1.9240504555340081,-3.1549714957080104) ;
        (3.6204658602769442,0.67923508276689581) ;
        (-7.0890928039911838,-2.1695635438689793) ;
        (-5.3648202955868056,1.050280285396771) ;
        (0.65321932931014,2.107965607603858) ;
        (-8.01607714852064,1.5623127541886062) ;
        (4.8542901105411858,2.1291352756884638) ;
        (5.4686576668506941,2.235612127663047) ;
        (-5.97805719045355,1.3148491907203042) ;
        (-8.896907563185767,-0.27106195782077114) ;
        (-0.59869952728367082,-0.15709118198554695) ;
        (1.0371687976403461,0.89491543849341681) ;
        (5.2247071457335394,3.3881885833012655) ;
        (-6.,3.) ;
        (-7.4212971870389222,-1.88465979513356) ;
        (-2.4228306104158932,-2.7367610368312478) ;
        (1.3914180338276747,0.9016961389494389) ;
        (-5.843197133621211,-1.0063781791989026)],

      [(-6.,-4.);(-2.,-3.5);(1.,-2.);(3.,-0.5);(7.,3.);(5.,4.);(3.,4.);(-2.,3.75);(-4.,3.5);(-6.,3.);(-8.,2.);(-10.,0.);(-10.,-1.);(-9.,-3.);(-8.,-3.5)]
      )

    |]

    let outputequal l1 l2 =
      List.length l1 = List.length l2 &&
      (List.zip_exn l1 l2 |> List.for_all ~f:(fun ((p1x,p1y),(p2x,p2y)) -> Float.(p1x = p2x && p1y = p2y)))

    let test1 testnum =
      if testnum < 1 || testnum > Array.length testdata then raise (Failure "Test not defined.") else
      let (testin,testout) = testdata.(testnum-1) in
      let time = Time.now () in
      let expout = CH.ch testin in
      let timetaken = Time.diff (Time.now ()) time |> Time.Span.to_ms in
      if outputequal expout testout
      then print_endline ("Test " ^ Int.to_string testnum ^ " passed in " ^ Float.to_string timetaken ^ " ms.")
      else raise (TestFailed testnum)

    let test () = for i = 1 to Array.length testdata do test1 i done

  end
