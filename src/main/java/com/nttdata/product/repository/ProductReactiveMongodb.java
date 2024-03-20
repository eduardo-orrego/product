package com.nttdata.product.repository;

import com.nttdata.product.model.Product;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

/**
 * Class: ProductReactiveMongodb. <br/>
 * <b>Bootcamp NTTDATA</b><br/>
 *
 * @author NTTDATA
 * @version 1.0
 *   <u>Developed by</u>:
 *   <ul>
 *   <li>Developer Carlos</li>
 *   </ul>
 * @since 1.0
 */
@Repository
public interface ProductReactiveMongodb extends ReactiveMongoRepository<Product, String> {

  Mono<Boolean> existsByType(String type);

  Mono<Product> findByType(String type);

}

