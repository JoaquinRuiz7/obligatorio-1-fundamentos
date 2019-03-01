-- Joaquin Ruiz , Sebastian del Arca:
-- 206164:            202096
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Ranas where 

data Rana where {V :: Rana ; M :: Rana}  deriving (Eq, Show)

type Estado = ([Rana],[Rana])

data Mov where {AM :: Mov ; SM :: Mov ; AV :: Mov ; SV :: Mov } deriving (Eq, Show)

inicial :: Estado
inicial = ([V,V,V],[M,M,M])


final :: Estado
final = ([M,M,M],[V,V,V])
--Recibe una lista de ranas y la transforma en lista de Strings.
mostra :: [Rana] -> [String]
mostra = \l -> case l of {
                 [] -> ["_"];
				 x:xs -> case x of {
				          V -> "V":(mostra xs);
						  M -> "M":(mostra xs);
				 
				 }; 
              
			 
}
--Muestra los caracteres de la lista de Strings concatenados .
mostrarListaString :: [String] -> String
mostrarListaString = \l -> case l of {
                [] -> "";
				x:xs -> case x of {
				        ['V'] -> "V" ++ mostrarListaString xs;
						['M'] -> "M" ++ mostrarListaString xs ;
						['_'] -> "_" ++ mostrarListaString xs;
				
				};

}



--Mostrar llama a Mostra que transforma la lista de [Ranas] a [String] y cuando esta vacia agrega el caracter "_" que representa la piedra (La coma).
--En mostrar cuando es vacia devuelve "" porque en mostra ya se represento la piedra vacia , entonces no es necesario hacerlo en mostrar (Sino mostraria dos piedras libres).
--Si no es vacia llama a mostrarListaString que hace exactamente lo que dice el nombre, luego concatena el primer elemento con el resto de los elementos que quedan y luego es llamado nuevamente para concatenar los caracteres de la segunda lista menos el ultimo , porque el ultimo representa la piedra y si no lo ignoramos mostraria dos piedras libres.
mostrar :: Estado -> String
mostrar = \e -> case e of {
            (l1,l2) -> case mostra l1 of {
			            [] -> "";
						x:xs -> x ++ mostrarListaString xs ++ mostrarListaString ( sacarUltimo (mostra l2))  ;
			
			}

}

valido :: Estado -> Mov -> Bool
valido = \e -> \m -> case m of {
                 --Avanza Verde
              AV -> case e of {
			        (l1,[]) -> case (largo l1) > 3 of {
					            True -> case ultimo l1 of {
								        V -> True;
										M -> False;
								
								};
								
					
					
					};
					(l1,l2) -> case l1 of {
					        [] -> False;
							x:xs ->  case ultimo l1 of {
							          V -> True;
									  M -> False;
							
						    };
					
					
				};
};
					--Salta Verde
					SV -> case e of {
					     ([],l2) -> False;
					     (l1,[]) -> case ultimo l1 of {
						                 M -> case sacarUltimo l1 of {
										          [] -> False;
												  x:xs -> case  ultimo xs  of {
												              V -> True;
															  M -> False;
												  
												  };
										 
										 
										 };
										 V -> False;
						 
						 
						 };
						 (l1,l2) -> case largo l1 of {
						 --Hay que hacer caso en largo 2 sino puede que de error de lista vacia al usar la funcion 
						             2 -> case ultimo l1 of {
									       M -> case sacarUltimo l1 of {
										             [] -> False;
													 p:ps -> case p of {
													     V -> True;
														 M -> False;
													 
													 };
										   
										   
										   };
									 
									 
									 };
									 y -> case ultimo l1 of {
									       M -> case sacarUltimo l1 of {
									           [] -> False;
											   y:ys -> case ultimo ys  of {
											              V -> True;
														  M -> False;
											   
											   };
									   
									   
									   };
									   V -> False;
									 
									 
									 };
									 
									 
						 
						 };
						 
					
					};
					--Avanza Marron
					AM -> case e of {
					(l1,[]) -> False;
					([],l2) -> case devolverPrimero l2 of {
					                  M -> True;
									  V -> False;
					
					
					};
					(l1,l2) -> case devolverPrimero l2 of {
					                  M -> True;
									  V -> False;
					
					             };
					
					
					
					
};
					--Salta  Marron
					SM -> case e of {
					  (l1,[]) -> False;
                       ([],l2) -> case devolverPrimero l2 of {
					                  V -> case sacarPrimero l2 of {
									             [] -> False;
												 z:zs -> case z of {
												                 M -> True;
																 V -> False;
												 
												 
												 };
									  
									  
									  };
									  M -> False;
					   
					   
					   };
					   (l1,l2) -> case devolverPrimero l2 of {
					                V -> case sacarPrimero l2 of {
									         [] -> False;
											 u:us -> case u of {
											                 M -> True;
															 V -> False;
											 
											 };
									
									
									};
									M -> False;
					   
					   
					   }; 
					
					}
					
					
					
					
			  }





       
--Devuelve la lista sin el primer elemento.	   
cola :: [a] -> [a]
cola = \l -> case l of {
                 [] -> error "Vacio";
				 x:xs -> xs;

}	    
          

          

sacarUltimo :: [a] -> [a]
sacarUltimo = \l -> case l of {
               [] -> error "Vacia";
			   x:xs ->  case xs of{
			         [] -> [];
					 y:ys ->  x :sacarUltimo xs;
			   }
}
ultimo :: [a] -> a 
ultimo = \l -> case l of {
            [] -> error "Vacia";
			x:xs -> case largo l of {
			          1 -> x;
					  n -> ultimo xs;
			}
}
sacarPrimero :: [a] -> [a]
sacarPrimero = \l -> case l of {
                [] -> error "Vacia";
				x:xs -> xs;				
				
}


largo :: [a] -> Int
largo = \l-> case l of {
          [] -> 0;
		  x:xs -> 1 + largo xs;
}

devolverPrimero :: [a] -> a
devolverPrimero = \l -> case l of {
                    [] -> error "Vacia";
					x:xs -> x;
}
devolverUltimo :: [Rana] -> Rana
devolverUltimo = \l -> case l of {
              [] -> error "Vacia";
			  x:xs -> case largo l of {
			          1 -> x;
					  _ -> devolverUltimo xs;
			  }; 

}
saltaM :: Estado -> Estado
saltaM = \e -> case valido e SM of {
                   True -> case e of {
				      (l1,l2) -> ( l1 ++ [devolverPrimero (sacarPrimero l2)] ++ [devolverPrimero l2 ],sacarPrimero(sacarPrimero l2))
				        
				   
				   };
				   False -> e;

}
saltaV :: Estado -> Estado
saltaV = \e -> case valido e SV of {
                True -> case e of {
			   (l1,l2) -> ( sacarUltimo (sacarUltimo l1)  ,  (ultimo l1) : ((ultimo (sacarUltimo l1))) : l2) ;
			 
			 };  
				False -> e;
           

		   }
		   

moverAM :: Estado -> Estado 
moverAM = \e -> case valido e AM of {
                  True -> case e of {
			          (l1,l2) -> (  l1 ++ [devolverPrimero l2]  ,  sacarPrimero l2 );
					  (l1,[]) -> (sacarUltimo l1 , [ultimo l1])
					  
			   };
				  
				  False -> e;

}

moverAV :: Estado -> Estado
moverAV = \e -> case valido e AV of {
               True -> case e of {
			          (l1,l2) -> (sacarUltimo l1 , ultimo l1 : l2 );
					  (l1,[]) -> (sacarUltimo l1 , [ultimo l1])
					  
			   };
			   False -> e;

}


mover :: Mov -> Estado ->  Estado
mover = \m -> \e -> case m of {
             AV -> moverAV e ;
			 SV -> saltaV e;
			 AM -> moverAM e;
			 SM -> saltaM e;
			 
			 
            
}

moves :: [Mov]
moves = [AM,AV,SV,SM]; 
--Movess devuelve una lista de los movimientos posibles junto con como quedaria el estado luego de aplicarlo.
movess :: [Mov] -> Estado -> [(Mov,Estado)]
movess = \m -> \e -> case m of {
             [] -> [];
			 x:xs -> case x of {
			   AV -> case valido e AV of {
			             True -> (AV,moverAV e) : movess xs e;
						 False -> movess xs e;
			   
			                 };
			   SV -> case valido e SV of {
			           True -> (SV, saltaV e) : movess xs e;
					   False -> movess xs e;
			   
			                };
			   AM -> case valido e AM of {
			          True -> (AM,moverAM e) : movess xs e;
					  False -> movess xs e;
			   
			              };
			   SM -> case valido e SM of {
			           True -> (SM,saltaM e) : movess xs e;
					   False -> movess xs e;
			   
			             };
			 
	};

}
-- Usa la funcion auxiliar Movess que recibe una lista con todos los posibles movimientos y el estado y devuelve la lista con posibles movimientos validos y estado resultante.
movimientos :: Estado -> [(Mov, Estado)]
movimientos = \e -> movess moves e ;
                



		 



gano :: Estado -> Bool
gano = \e -> case e of {
        ([M,M,M],[V,V,V]) -> True;
		_ -> False;
}
--LLama a Movimientos y si no queda ningun movimiento para hacer significa que perdio.
perdio :: Estado -> Bool
perdio = \e -> case movimientos e of {
                  [] -> True;
                   _ -> False;
}